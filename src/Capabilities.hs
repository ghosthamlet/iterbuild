{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Capabilities where

import Protolude hiding (writeFile, readFile, (%))
import System.Directory
import System.Directory.Tree
import System.FilePath (combine, addExtension)
import qualified System.IO
import System.IO.Temp
import Path
import Formatting hiding (build)
import Shelly hiding ((</>), find)

import Data.Text.Lazy (splitOn)
import Data.Text.Lazy.IO
import Data.Functor.Compose
import Control.Monad.Catch

import Expr

-- We will a function for hashing directories in the format of the directory-tree library
 -- The following function is copied from https://github.com/jberryman/directory-tree
 -- HELPER: a non-recursive comparison
comparingConstr :: DirTree a -> DirTree a1 -> Ordering
comparingConstr (Failed _ _) (Dir _ _)    = LT
comparingConstr (Failed _ _) (File _ _)   = LT
comparingConstr (File _ _) (Failed _ _)   = GT
comparingConstr (File _ _) (Dir _ _)      = GT
comparingConstr (Dir _ _)    (Failed _ _) = GT
comparingConstr (Dir _ _)    (File _ _)   = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')

-- We want to sort the keys in a reasonable fashion before hashing the tree.
deriving instance Generic a => Generic (DirTree a)
instance (Generic a, Hashable a) => Hashable (DirTree a) where
    hashWithSalt salt (File name contents) = hashWithSalt salt (name, contents)
    hashWithSalt salt (Dir name files) = hashWithSalt salt (name, sortBy comparingConstr files)
    hashWithSalt salt (Failed name err) = hashWithSalt salt name


-- These are the first 2 layers of my 3 layer cake https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- The amount of classes here should be minimized, as it is tiresome to write that code. I love just how
-- you can use polymorphism to dump all your 'imperative' code(layer 1) into one file and then just forget about it.

class HasTargetPath f where
    targetPath :: f (Path Abs Dir)
    -- Args: optional label, optional file extension, contents
    
    -- Just hashes the text and somehow embedds that information into the path. The resulting
    -- filename gets returned. It also checks whether the file exists first. This is mainly used by
    -- very common snippets that are used for glueing code snippets and don't need to be stored
    -- in a git repository.

    -- At some later point, I might add 'ressource dirs', where the user can
    -- define such gluing snippets and refer to them later, but for now this is sufficient since this
    -- would mean that I need to isolate a 'naming function' that then gets used by addContent and the
    -- new function, though I will probably be able to keep this type-class interface so I am fine.

    -- Guideline: Write your code such that files that are used often are named uniformly by hardcoding
    -- the labels into the code.

    -- I think it makes sense to use a maybe here as opposed to 2 different functions, because as I wrote more code,
    -- it got very painful having to always write an extra function.
    addContent :: f (Maybe LText) -> f (Maybe LText) -> f LText -> f (Path Rel File)

type GitRef = LText

class HasTargetPath f => HasGit f where
    gitRepoPath :: f (Path Abs Dir)
    -- This should really only be a commit to ensure reproducibility and thus enable having clear hashes for computations.
    -- This means I can check whether a python object has been pre-computed and then possibly provide the python object
    -- from cache. (i.e. pass an argument like read_cache("4f23ea32ef534e") that outputs a pandas object by reading from cache.)
    fetchCommit :: f LText -> f (Path Rel Dir)

class HasCache f where
    cachePath :: f (Path Abs Dir)
    tmpPath :: f (Path Abs Dir)
    -- First argument is competition, second one is a hash. Output is the path where data was cached or a new path where
    -- the data was downloaded.
    fetchKaggle :: f LText -> f LText -> f (Path Rel Dir)

data Env = Env {
    getTargetPath :: Path Abs Dir,
    -- Setting this to /tmp may be bad because the size of /tmp may not suffice for unzipping large files
    getTmpPath :: Path Abs Dir,
    getGitRepoPath :: Path Abs Dir,
    getCachePath :: Path Abs Dir,
    getResultPath :: Path Abs Dir
}

newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

setDefaultPaths :: BaseMonad a -> IO a
setDefaultPaths b =
    do
        currentDir <- getCurrentDirectory
        env <- Env
                <$> parseAbsDir (combine currentDir "target/")
                <*> parseAbsDir (combine currentDir ".git/")
                <*> parseAbsDir (combine currentDir "cache/")
                <*> parseAbsDir (combine currentDir "results/")
                <*> parseAbsDir (combine currentDir "tmp/")
        runReaderT (getBaseMonad b) env

-- TODO: This does not produce a fixed size hash, which is stupid.
hashFunction :: Hashable a => a -> LText
hashFunction = format (base 36) . hash

formatFileName :: (MonadThrow m, Hashable a) => Maybe LText -> Maybe LText -> a -> m (Path Rel File)
formatFileName label extension =
    parseRelFile
    . toS
    . maybe identity (flip $ format $ text % "." % text) extension
    . maybe identity (format $ text % "-" % text) label
    . hashFunction

formatDirName :: (MonadThrow m, Hashable a) => Maybe LText -> a -> m (Path Rel Dir)
formatDirName label =
    parseRelDir
    . toS
    . (<> "/")
    . maybe identity (format $ text % "-" % text) label
    . hashFunction

-- The more general version causes overlapping instances later on.
-- instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasTargetPath m where
instance HasTargetPath BaseMonad where
    targetPath = asks getTargetPath
    addContent label extension content =
        do
            label <- label
            extension <- extension
            content <- content
            filename <- formatFileName label extension content
            targetPath <- targetPath
            let path = toFilePath (targetPath </> filename)
            
            exists <- (liftIO . doesFileExist) path
            bool (liftIO $ writeFile path content) (pure ()) exists
            return filename


-- instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasGit m where
instance HasGit BaseMonad where
    gitRepoPath = asks getGitRepoPath
    fetchCommit commit =
        -- check for existence?
        do
            commit <- commit
            pathEnd <- parseRelDir (toS commit)
            writePath <- (</> pathEnd) <$> targetPath
            shelly $ escaping False $ run "git" ["archive", toS commit, "| tar -x -C", "git-" <> toS (toFilePath writePath)]
            return pathEnd


-- Puts an external file, whose hash is yet unknown into the local database. It does not return the path where
-- it is put into to force the user to explicitly lookup the hash to ensure reproducibility. 
importLocalFile :: BaseMonad (Path Abs File) -> BaseMonad (Path Rel File)
importLocalFile source =
    do
        source <- source
        targetName <- setFileExtension "" . filename $ source
        contents <- liftIO . readFile . toFilePath $ source
        filename <- formatFileName (Just . toS . toFilePath $ targetName) (Just . toS . fileExtension $ source) contents

        cachePath <- cachePath
        liftIO (renameFile (toFilePath source) (toFilePath (cachePath </> filename)))
        return filename

importLocalDir :: BaseMonad (Path Abs Dir) -> BaseMonad (Path Rel Dir)
importLocalDir source =
    do
        source <- source
        directoryContents <- liftIO . readDirectoryWithL readFile . toFilePath $ source
        let a = hash <$> directoryContents

        contents <- liftIO . readFile . toFilePath $ source
        dirname <- formatDirName (Just . toS . toFilePath . dirname $ source) contents

        cachePath <- cachePath
        liftIO (renameFile (toFilePath source) (toFilePath (cachePath </> dirname)))
        return dirname

newtype CacheLookupException = CacheLookupException { key :: LText } deriving (Show)

instance Exception CacheLookupException

-- This is used when you want to guarantee your data having some hash like when the data was imported with
-- importLocalFile. This is a nice solution for dealing with outside data and still having reproducibility
-- guarantees.
findCached_ :: (FilePath -> IO Bool) -> BaseMonad LText -> BaseMonad FilePath
findCached_ predicate hash =
    do
        cachePath <- cachePath
        hash <- hash

        allpaths <- liftIO . listDirectory . toFilePath $ cachePath
        filtered <- liftIO . foldlM (\acc next -> ifM (predicate next) (return (next:acc)) (return acc)) [] $ allpaths
        let paths = fmap toS filtered

        filepath <- case find ((== Just hash) . head . splitOn "-") paths of
            Just x -> return x
            Nothing -> throwM (CacheLookupException hash)
        return (toS filepath)

findCachedFile :: BaseMonad LText -> BaseMonad (Path Rel File)
findCachedFile hash = findCached_ doesFileExist hash >>= parseRelFile

findCachedDir :: BaseMonad LText -> BaseMonad (Path Rel Dir)
findCachedDir hash = findCached_ doesDirectoryExist hash >>= parseRelDir

fetchKaggle_ :: BaseMonad LText -> BaseMonad (Path Rel Dir)
fetchKaggle_ competition =
    do
        competition <- competition
        shelly . verbosely . print_stdout True . onCommandHandles (initOutputHandles (`System.IO.hSetBinaryMode` True)) $
            run "kaggle" ["competitions", "download", "-c", toS competition, "-p", "/tmp/"]
        
        let dir = ((++ "/") . toS) competition
        shelly $ run "unzip" [toS . combine "/tmp" . (`addExtension` "zip") . toS $ competition, "-d", toS . combine "/tmp" $ dir]

        cachePath <- cachePath

        liftIO $ renameDirectory (combine "/tmp" dir) (combine (toFilePath cachePath) dir)
        -- importLocalFile (parseAbsFile $ combine "/tmp" (toS competition))
        undefined


instance HasCache BaseMonad where
    cachePath = asks getCachePath
    tmpPath = asks getTmpPath
    fetchKaggle = undefined
downloadKaggle competition =
    do
        competition <- competition
        shelly . verbosely . print_stdout True . onCommandHandles (initOutputHandles (`System.IO.hSetBinaryMode` True)) $
            run "kaggle" ["competitions", "download", "-c", toS competition, "-p", "/tmp/"]
        
        let dir = ((++ "/") . toS) competition
        shelly $ run "unzip" [toS . combine "/tmp" . (`addExtension` "zip") . toS $ competition, "-d", toS . combine "/tmp" $ dir]

        cachePath <- cachePath

        liftIO $ renameDirectory (combine "/tmp" dir) (combine (toFilePath cachePath) dir)
        importLocalFile (parseAbsFile $ combine "/tmp" (toS competition))


-- What the fuck, why do I need to write this trivial code? There surely is a better
-- way to do this, but with the :.: operator, I can't get some code to compile which
-- compiles if you manually compose.
instance Applicative f => HasTargetPath (Compose f BaseMonad) where
    targetPath = Compose $ pure targetPath
    addContent x y z = Compose $ liftA3 addContent (getCompose x) (getCompose y) (getCompose z)
-- Note that this function uses map implicitly in liftA2, so it has no possibility of being replaced. This probably is fine,
-- since the input expression will already be named.

instance Applicative f => HasGit (Compose f BaseMonad) where
    gitRepoPath = Compose $ pure gitRepoPath
    fetchCommit = Compose . fmap fetchCommit . getCompose