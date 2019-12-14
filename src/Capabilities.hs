{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Capabilities where

import Path
import Protolude hiding (writeFile)
import System.Directory
import System.FilePath (combine)
import Data.Text.Lazy.IO
import Formatting
import Data.Functor.Compose
import Control.Monad.Catch
import Expr


-- The cool thing is, that now an applicative that has the capabilities below has no access to
-- a general IO monad, only to these restricted functions. This is a typical haskell example of using
-- polymorphism to provide stronger guarantees about code and I find this extremely cool
class HasTargetPath f where
    targetPath :: f (Path Abs Dir)
    -- Given an optional label and some text you want to just add to the targetPath,
    -- this just hashes the text and somehow embedds that information into the path. The resulting
    -- filename returned. It also checks whether the file exists first. This is mainly used by
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
    addContent :: f (Maybe LText) -> f LText -> f (Path Rel File)

class HasTargetPath f => HasGit f where

data Env = Env {
    getTargetPath :: Path Abs Dir,

    getGitRepoPath :: Path Abs Dir,
    getGitPath :: Path Abs File,

    getDvcPath :: Path Abs File,
    getDvcCache :: Path Abs Dir,

    getResultPath :: Path Abs Dir
}

newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

nameFile :: Hashable a => Maybe LText -> a -> LText
nameFile Nothing = format hex . hash
nameFile (Just text) = ((<>) text) . nameFile Nothing

instance (MonadIO m, MonadThrow m, MonadReader Env m) => HasTargetPath m where
    targetPath = asks getTargetPath
    addContent label content =
        do
            label <- label
            content <- content
            let name = nameFile label content
            targetPath <- targetPath
            let path = toFilePath targetPath ++ (toS name)
            
            exists <- (liftIO . doesFileExist) path
            bool (liftIO $ writeFile path content) (pure ()) exists
            parseRelFile (toS name)

-- What the fuck, why do I need to write this trivial code? There surely is a better
-- way to do this, but with the :.: operator, I can't get some code to compile which
-- compiles if you manually compose.
instance Applicative f => HasTargetPath (Compose f BaseMonad) where
    targetPath = Compose $ pure targetPath
    addContent x y = Compose $ liftA2 addContent (getCompose x) (getCompose y)
-- Note that this function uses map implicitly in liftA2, so it has no possibility of being replaced. This probably is fine,
-- since the input expression will already be named.
