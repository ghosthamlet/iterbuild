{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Capabilities where

import Protolude hiding (writeFile, readFile, (%), hash, take, first)
import System.IO
import qualified GHC.Show
import System.FilePath (combine, addExtension, makeRelative)
import qualified System.IO
import Crypto.Hash (Context, Digest, SHA256, hash, hashInit, hashUpdate, hashFinalize)
import Path
import qualified Path.IO as PIO
import Formatting hiding (build)
import Shelly hiding ((</>), find)

import Data.Text.Lazy (splitOn, take, dropEnd)
import qualified Data.ByteString as B
import Data.Functor.Compose
import Control.Monad.Catch
import Control.Arrow

import Expr

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
    addContent :: f (Maybe LText) -> f (Maybe LText) -> f ByteString -> f (Path Rel File)

type GitRef = LText

class HasTargetPath f => HasGit f where
    gitRepoPath :: f (Path Abs Dir)
    -- This should really only be a commit to ensure reproducibility and thus enable having clear hashes for computations.
    -- This means I can check whether a python object has been pre-computed and then possibly provide the python object
    -- from cache. (i.e. pass an argument like read_cache("4f23ea32ef534e") that outputs a pandas object by reading from cache.)
    fetchCommit :: f LText -> f (Path Rel Dir)

data UnexpectedHash = 
    Kaggle { competition :: LText }

instance Show UnexpectedHash where
    show e = toS .
        (format ("Got an unexpected hash while downloading the kaggle competition '" % text % "'. This means the data of the competition has changed.")) $ (competition e)

instance Exception UnexpectedHash

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
    getResultPath :: Path Abs Dir,
    getHashLength :: Int64
}

newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow, MonadCatch, MonadMask)

lowerIO :: Path Abs Dir -> Int64 -> BaseMonad a -> IO a
lowerIO baseDir hashLength b =
    do
        target <- parseRelDir "target/"
        tmp <- parseRelDir "tmp/"
        git <- parseRelDir ".git/"
        cache <- parseRelDir "cache/"
        results <- parseRelDir "results/"
        
        let env = Env (baseDir </> target)  (baseDir </> tmp) (baseDir </> git) (baseDir </> cache) (baseDir </> results) hashLength
        mapM_ (PIO.createDirIfMissing False . (baseDir </>)) [target, tmp, cache, results]
        runReaderT (getBaseMonad b) env

withDefaults :: BaseMonad a -> IO a
withDefaults b = PIO.getCurrentDir >>= (\dir -> lowerIO (parent dir) 7 b)

hashFn :: ByteString -> Digest SHA256
hashFn = hash

hashFile :: MonadIO m => Path a File -> m (Digest SHA256)
hashFile = fmap hashFn . liftIO . B.readFile . toFilePath

foldHash :: [ByteString] -> Digest SHA256
foldHash = hashFinalize . foldl hashUpdate hashInit

-- maybe the contents should be hashed in parallel
hashDir :: MonadIO m => Path a Dir -> m (Digest SHA256)
hashDir source = do
        paths <- PIO.listDirRecurRel source
        contents <- liftIO $ mapM (B.readFile . toFilePath . (source </>)) (snd paths)
        return $ foldHash ((toS . toFilePath <$> fst paths) ++ (toS . toFilePath <$> snd paths) ++ contents)

formatFileName :: MonadThrow m => Maybe LText -> Maybe LText -> Int64 -> Digest a -> m (Path Rel File)
formatFileName label extension hashLength =
    maybe identity (\ext -> (>>= addFileExtension (toS ext))) extension
    . parseRelFile
    . toS
    . maybe identity (format $ text % "-" % text) label
    . take hashLength
    . show

formatDirName :: MonadThrow m => Maybe LText -> Int64 -> Digest a -> m (Path Rel Dir)
formatDirName label hashLength =
    parseRelDir
    . toS
    . (<> "/")
    . maybe identity (format $ text % "-" % text) label
    . take hashLength
    . show

-- The more general version causes overlapping instances later on.
-- instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasTargetPath m where
instance HasTargetPath BaseMonad where
    targetPath = asks getTargetPath
    addContent label extension content =
        do
            label <- label
            extension <- extension
            content <- content
            hashLength <- asks getHashLength
            filename <- formatFileName label extension hashLength (hashFn content)
            targetPath <- targetPath
            let path = targetPath </> filename
            
            exists <- (liftIO . PIO.doesFileExist) path
            bool (liftIO $ B.writeFile (toFilePath path) content) (pure ()) exists
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
importLocalFile :: Path Abs File -> BaseMonad (Digest SHA256, Path Rel File)
importLocalFile source =
    do
        targetName <- setFileExtension "" . filename $ source
        digest <- hashFile source
        hashLength <- asks getHashLength
        -- TODO: this does not deal well with multiple extensions
        filename <- formatFileName (Just . toS . toFilePath $ targetName) (Just . toS . fileExtension $ source) hashLength digest

        cachePath <- cachePath
        liftIO (PIO.renameFile source (cachePath </> filename))
        return (digest, filename)

importLocalDir :: Path Abs Dir -> BaseMonad (Digest SHA256, Path Rel Dir)
importLocalDir source =
    do
        digest <- hashDir source

        hashLength <- asks getHashLength
        dirname <- formatDirName (Just . dropEnd 1 . toS . toFilePath . dirname $ source) hashLength digest

        cachePath <- cachePath
        liftIO (PIO.renameDir source (cachePath </> dirname))
        return (digest, dirname)


-- This is used when you want to guarantee your data having some hash like when the data was imported with
-- importLocalFile. This is a nice solution for dealing with outside data and still having reproducibility
-- guarantees.
findCached_ :: (Path Abs Dir -> BaseMonad [Path Rel a]) -> LText -> BaseMonad (Maybe (Path Rel a))
findCached_ listPaths hash =
    do
        -- TODO: What about nonhomogeneous hash lengths?
        cachePath <- cachePath
        paths <- listPaths cachePath

        return . find ((== Just hash) . head . splitOn "-" . toS . toFilePath) $ paths


findCachedFile :: LText -> BaseMonad (Maybe (Path Rel File))
findCachedFile = findCached_ (fmap snd . PIO.listDirRel)

findCachedDir :: LText -> BaseMonad (Maybe (Path Rel Dir))
findCachedDir = findCached_ (fmap fst . PIO.listDirRel)

fetchKaggle_ :: LText -> BaseMonad (Digest SHA256, Path Rel Dir)
fetchKaggle_ competition =
    let fetchKaggle__ tmpPath = do
            shelly $ run "kaggle" ["competitions", "download", "-c", toS competition, "-p", toS $ toFilePath tmpPath]
            
            directory <- parseRelDir $ ((++ "/") . toS) competition
            shelly $ run "unzip" [
                    toS . combine (toFilePath tmpPath) . (`addExtension` "zip") . toS $ competition,
                    "-d",
                    toS $ toFilePath $ tmpPath </> directory
                ]

            importLocalDir (tmpPath </> directory)
    in
        asks getTmpPath >>= (\tmp -> PIO.withTempDir tmp (toS competition) fetchKaggle__)

checkedFetchKaggle :: LText -> LText -> BaseMonad (Path Rel Dir)
checkedFetchKaggle competition expectedHash =
    do
        (digest, path) <- fetchKaggle_ competition
        when (expectedHash /= show digest) $ throwM (Kaggle competition)
        return path

instance HasCache BaseMonad where
    cachePath = asks getCachePath
    tmpPath = asks getTmpPath
    fetchKaggle competition expectedHash =
        fromMaybe <$> (join $ (liftM2 checkedFetchKaggle) competition expectedHash) <*> (expectedHash >>= findCachedDir)


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