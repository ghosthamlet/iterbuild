{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Capabilities where

import System.FilePath
import Data.Hashable
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Monad.Reader
import Turtle (mkdir, output)
import Expr

script :: IO ()
script = do
    mkdir "test"
    output "test/file.txt" "Hello"

data Env = Env {
    fileNamer :: forall b. Hashable b => b -> String -> String,
    targetPathConst :: FilePath
}

newtype BaseMonad a = BaseMonad { getBaseMonad :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

newtype BaseExpr a = BaseExpr { getExpr :: Compose (FreeA Labeled) BaseMonad a }
    deriving (Functor, Applicative, HasFileNamer, HasTargetPath, HasTargetWriter)

-- here are some generic sample implementations for tipical types
getFileNamer :: MonadReader Env m => forall b. Hashable b => b -> m (String -> String)
getFileNamer hashable = asks (`fileNamer` hashable)

getTargetPath :: MonadReader Env m => m String
getTargetPath = asks targetPathConst

-- first argument is the relative path to the target path and second one the contents
getTargetWriter :: (MonadIO m, MonadReader Env m) => FilePath -> String -> m ()
getTargetWriter pathSuffix contents = 
    do
        targetPath <- getTargetPath
        liftIO $ writeFile contents (targetPath </> pathSuffix)

-- Lets define the atomic components of our applicative!
liftKleisli :: Monad m => (a -> m b) -> Compose Expr m a -> Compose Expr m b
liftKleisli f = Compose . fmap (>>= f) . getCompose

-- The cool thing is, that now an applicative that has the capabilities below has no access to
-- a general IO monad, only to these restricted functions. This is a typical haskell example of using
-- polymorphism to provide stronger guarantees about code and I find this extremely cool.
-- Also note that the polymorphic applicatives with the properties below, need not have anything to
-- do with replaceable expressions - these are just an implementation detail.
class HasFileNamer f where
    -- f a -> f b is seriously much much easier to work with than f (a -> b). You want to forget you are
    -- even applying an applicative in first place and pretend these are values.
    nameFile :: Hashable a => f a -> f String -> f String
instance MonadReader Env m => HasFileNamer (Compose Expr m) where
    nameFile = (<*>) . liftKleisli getFileNamer

class HasTargetPath f where
    targetPath :: f String
instance MonadReader Env m => HasTargetPath (Compose Expr m) where
    targetPath = Compose $ pure getTargetPath

-- unwrapTuple :: Functor f => f (a,b) -> (f a, f b)
-- unwrapTuple x = (fst <$> x, snd <$> x)

wrapTuple :: Applicative f => f a -> f b -> f (a,b)
wrapTuple = (<*>) . fmap (curry id)

class HasTargetWriter f where
    writeTarget :: f FilePath -> f String -> f ()
instance (MonadIO m, MonadReader Env m) => HasTargetWriter (Compose Expr m) where
    writeTarget = curry (liftKleisli (uncurry getTargetWriter) . uncurry wrapTuple)
