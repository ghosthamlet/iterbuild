{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Capabilities where

import Protolude hiding (writeFile, (%))
import System.Directory
import System.FilePath (combine)
import Path
import Formatting
import Shelly hiding ((</>))

import Data.Text.Lazy.IO
import Data.Functor.Compose
import Control.Monad.Catch

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

data Env = Env {
    getTargetPath :: Path Abs Dir,
    getGitRepoPath :: Path Abs Dir,
    getCachePath :: Path Abs Dir,
    getResultPath :: Path Abs Dir
}

newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

nameFile :: Hashable a => Maybe LText -> Maybe LText -> a -> LText
nameFile label extension =
    maybe identity (flip $ format $ text % "." % text) extension
    . maybe identity (format $ text % "-" % text) label
    . format hex
    . hash

-- The more general version causes overlapping instances later on.
-- instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasTargetPath m where
instance HasTargetPath BaseMonad where
    targetPath = asks getTargetPath
    addContent label extension content =
        do
            label <- label
            extension <- extension
            content <- content
            let name = nameFile label extension content
            targetPath <- targetPath
            let path = toFilePath targetPath ++ (toS name)
            
            exists <- (liftIO . doesFileExist) path
            bool (liftIO $ writeFile path content) (pure ()) exists
            parseRelFile (toS name)


-- instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasGit m where
instance HasGit BaseMonad where
    gitRepoPath = asks getGitRepoPath
    fetchCommit commit =
        -- check for existence?
        do
            commit <- commit
            pathEnd <- parseRelDir (toS commit)
            writePath <- (</> pathEnd) <$> targetPath
            shelly $ escaping False $ run "git" ["archive", toS commit, "| tar -x -C", "git-" <> (toS $ toFilePath writePath)]
            return pathEnd

instance HasCache BaseMonad where
    cachePath = asks getCachePath

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