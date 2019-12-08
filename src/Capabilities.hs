{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

newtype DefaultExpr a = ExprImpl { getExpr :: Compose (FreeA Labeled) BaseMonad a }
    deriving (Functor, Applicative)

-- Here I list the capabilities I want the base monad to have, which will later be used
-- to define the "atoms" of my DSL. The cool thing is, that now the DSL typeclass has no access to
-- a general IO monad, only to these restricted functions. This is a typical haskell example of using
-- polymorphism to provide stronger guarantees about code and I find this extremely cool.
class HasFileNamer m where
    getFileNamer :: forall b. Hashable b => b -> m (String -> String)
-- This instance would have worked for any (Monad m, MonadReader Env m), but if I do that then I need
-- undecidable instances and I get warnings about fragile type checking later on
instance MonadReader Env m => HasFileNamer m where
    getFileNamer hashable = asks (`fileNamer` hashable)

class HasTargetPath f where
    getTargetPath :: f String
instance MonadReader Env m => HasTargetPath m where
    getTargetPath = asks targetPathConst

class HasTargetWriter m where
    -- first argument is the relative path to the target path and second one the contents
    getTargetWriter :: FilePath -> String -> m ()
instance (MonadIO m, HasTargetPath m) => HasTargetWriter m where
    getTargetWriter pathSuffix contents = 
        do
            targetPath <- getTargetPath
            liftIO $ writeFile contents (targetPath </> pathSuffix)

-- expression post-composed to monad, this toghether with constraints on the
-- capabilities of the monad m will be our 'polymorphic expression' with hidden IO
type ExprM m = Compose Expr m

-- Lets define the atoms of our DSL!
liftKleisli :: Monad m => (a -> m b) -> ExprM m a -> ExprM m b
liftKleisli f = Compose . fmap (>>= f) . getCompose

nameFile :: (Monad m, HasFileNamer m, Hashable a) => ExprM m a -> ExprM m (String -> String)
nameFile = liftKleisli getFileNamer

targetPath :: (Monad m, HasTargetPath m) => ExprM m FilePath
targetPath = Compose $ pure getTargetPath

writeTarget :: (Monad m, HasTargetWriter m) => ExprM m (FilePath, String) -> ExprM m ()
writeTarget = liftKleisli $ uncurry getTargetWriter
