{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | These are the first 2 layers of my <https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html 3 layer cake>.
-- The amount of classes here should be minimized, as it is tiresome to write that code. I like how you can use polymorphism
-- to dump all your imperative code(layer 1) into one file and then just forget about it.
module Capabilities(
    HasTargetPath (..),
    UnexpectedHash (..),
    HasCache (..),
    BaseMonad (..),
    lowerIO,
    withDefaults,
    fetchKaggle_
) where

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

import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import Data.Functor.Compose
import Control.Monad.Catch
import Control.Arrow

import Expr

-- | Applicatives with the ability to write the build output to a target path.
class HasTargetPath f where
    -- | Path where the build output should be dumped to.
    targetPath :: f (Path Abs Dir)
    -- | Args: optional label, optional file extension(eg. ".txt", with the "."), contents
    --
    -- This writes the ByteString into a file in the @ targetPath @. The file name will be returned
    -- as a path relative to the @ targetPath @ and it will contain the label and extension
    -- information in addition to a hash of the ByteString
    -- 
    -- It is also checked whether the file exists first. This is mainly used by
    -- very common snippets that are used for glueing code snippets and don't need to be stored
    -- in a git repository.

    -- Guideline: Write your code such that files that are used often are named uniformly by hardcoding
    -- the labels into the code.

    -- TODO: At some later point, I might add 'ressource dirs', where the user can
    -- define such gluing snippets and refer to them later, but for now this is sufficient since this
    -- would mean that I need to isolate a 'naming function' that then gets used by addContent and the
    -- new function, though I will probably be able to keep this type-class interface so I am fine.
    addContent :: f (Maybe LText) -> f (Maybe LText) -> f ByteString -> f (Path Rel File)

-- | Applicatives with the ability to access git. Notice the absence of a function for fetching from a branch -
-- this is no mistake. Fetching from a branch yields different results depending on the state of the git repo,
-- but we would like the contents of our path to always be the same in order to ensure reproducibility.
class HasTargetPath f => HasGit f where
    -- | Location of the git repository from which to retrieve the code.
    gitRepoPath :: f (Path Abs Dir)
    -- | Fetches a commit from the git repo, returning the path(relative to 'gitRepoPath') the contents of the commit
    -- were written to.
    fetchCommit :: f LText -> f (Path Rel Dir)

-- | This exception occurs when attempting to import an external source to the cache, but the external source
-- has changed (and thus its hash has also changed). We check the hashes because we want to ensure reproducibility.
data UnexpectedHash = 
    Kaggle { competition :: LText }

instance Show UnexpectedHash where
    show e = toS .
        (format ("Got an unexpected hash while downloading the kaggle competition '" % text % "'. This means the data of the competition has changed.")) $ (competition e)

instance Exception UnexpectedHash

-- | This is used for importing external data and the code that gets built also uses this path to cache results of expensive computations.
class HasCache f where
    -- | Path to use as cache.
    cachePath :: f (Path Abs Dir)
    -- | Path where to generate temporary files and directories. Used when importing data from external sources.
    tmpPath :: f (Path Abs Dir)
    -- | First argument is competition, second one is a hexadecimal hash. Output is the path where data was cached or a new path where
    -- the data was downloaded. This function checks whether a directory with the given hash already exists, if it does not, it fetches
    -- the competition data from kaggle and checks whether the fetched data has the expected hash. If it does not, it throws a
    -- @ UnexpectedHash @ exception.
    --
    -- This function is intended to be in tandem with the iterbuild command line tool. If you need to write a function depending on data
    -- from a kaggle competition, you will first need to run @ iterbuild prefetch --kaggle YOURCOMPETITION @. This will print out the hash
    -- you will want to pass to fetchKaggle(you may also pass only the first n characters of the full hash). After this step has been done,
    -- you will never need to run this command again - even if you delete the cache, the file will be downloaded again automatically.
    --
    -- This is done this way in order to ensure reproducibility.
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

-- | Data structure defined with mtl, implementing all the above capabilities.
newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Fixes the various parameters in the reader monad with sane defaults. The first argument is a base path
-- in which all other paths will be generated(and the .git/ directory is expected to be there) and the second
-- argument determines the hash length. Note that changing the hash length for an existing project will cause
-- iterbuild to ignore the things that were cached before. This may get fixed at some point.
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

-- | Fixes the various parameters in the reader monad assuming standard directory structure. This means that your current
-- directory is the place were the main branch is checked out, but the actual .git dir and the iterbuild internal directories
-- are in the parent directory of the current directory. The default hash length is 7, yielding a probability of < 10^(-8) of
-- two hashes being the same.
withDefaults :: BaseMonad a -> IO a
withDefaults b = PIO.getCurrentDir >>= (\dir -> lowerIO (parent dir) 7 b)

hashFn :: ByteString -> Digest SHA256
hashFn = hash

hashFile :: MonadIO m => Path a File -> m (Digest SHA256)
hashFile = fmap hashFn . liftIO . BS.readFile . toFilePath

foldHash :: [ByteString] -> Digest SHA256
foldHash = hashFinalize . foldl hashUpdate hashInit

-- Maybe the contents should be hashed in parallel
hashDir :: MonadIO m => Path a Dir -> m (Digest SHA256)
hashDir source = do
        paths <- PIO.listDirRecurRel source
        contents <- liftIO $ mapM (BS.readFile . toFilePath . (source </>)) (snd paths)
        return $ foldHash ((toS . toFilePath <$> fst paths) ++ (toS . toFilePath <$> snd paths) ++ contents)

-- Checks whether the input hashes match on the first min(len(t1), len(t2)) characters.
hashEq :: LText -> LText -> Bool
hashEq t1 t2 = all (uncurry (==)) $ LT.zip t1 t2

formatFileName :: MonadThrow m => Maybe LText -> Maybe LText -> Int64 -> Digest a -> m (Path Rel File)
formatFileName label extension hashLength =
    maybe identity (\ext -> (>>= addFileExtension (toS ext))) extension
    . parseRelFile
    . toS
    . maybe identity (format $ text % "-" % text) label
    . LT.take hashLength
    . show

formatDirName :: MonadThrow m => Maybe LText -> Int64 -> Digest a -> m (Path Rel Dir)
formatDirName label hashLength =
    parseRelDir
    . toS
    . (<> "/")
    . maybe identity (format $ text % "-" % text) label
    . LT.take hashLength
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
            bool (liftIO $ BS.writeFile (toFilePath path) content) (pure ()) exists
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
        dirname <- formatDirName (Just . LT.dropEnd 1 . toS . toFilePath . dirname $ source) hashLength digest

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

        return . find ((== Just hash) . head . LT.splitOn "-" . toS . toFilePath) $ paths


findCachedFile :: LText -> BaseMonad (Maybe (Path Rel File))
findCachedFile = findCached_ (fmap snd . PIO.listDirRel)

findCachedDir :: LText -> BaseMonad (Maybe (Path Rel Dir))
findCachedDir = findCached_ (fmap fst . PIO.listDirRel)

-- | Fetches competition data from kaggle, returning a pair of the data directory hash(after unzipping) and the path
-- relative to the cache path where the directory lies.
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