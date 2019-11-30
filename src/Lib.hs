
{-# LANGUAGE DeriveFunctor #-}

module Lib where

import Control.Monad.Free
import Control.Comonad.Cofree
-- import Control.Monad.Trans    (MonadIO, liftIO)

newtype GitPtr = GitPtr String
newtype DirRef = DirRef String
newtype PyMod = PyMod String

-- DSL functor, represents a "step" in the DSL
data DSLF k =
  PyFun String DirRef (PyMod -> k) -- here the DirRef may come statically from the program or dynamically from a monad
  | GitCheckout GitPtr (DirRef -> k) deriving (Functor)
  -- | GetDVCPipe String [String] k -- takes command, output files
  -- | RegisterMetric DirRef (DirRef -> k)

-- The free monad transformer adds "recursiveness" to our definition. This could have been easily done manually, but now the DSL
-- can be extended by eg. forming coproducts with other functors
type DSL a = Free DSLF a
-- type DSLT m a = FreeT DSLF m a

-- Kleisli composition the following functions yields parsable expressions
-- So it is possible to have a typed DSL, thing is it is only extensible with the effort we are doing here
-- I wonder whether free applicatives solve all my problems
-- With this construction right here I need to add a new part to the DSL functor
getFun :: String -> DirRef -> DSL PyMod
getFun name ref = liftF $ PyFun name ref id
checkout :: GitPtr -> DSL DirRef
checkout ptr = liftF $ GitCheckout ptr id
-- etc.

-- Lol, the duality between arrow and product is real. What does this have to do with the adjoint situation `- x b  |-  - ^ b`?
-- This is not as intuitive to understand as Free. Why is k returned?
-- Cofree will be a sequence a, CoDLSF a, CoDSLF^2 a, ... - but this full generality will not be used anyways, since 
-- Elements of cofrees are constructed by an `a` and an `a -> F a`. So we need to provide an initial state and a function basically of the form
-- a -> In -> (Out, a), which is basically a pure computation coupled with a state manipulation inside the "comonad container".
data CoDSLF k = CoDSLF {
  coPyFun :: String -> DirRef -> (PyMod, k),
  coCheckout :: GitPtr -> (DirRef, k)
} deriving (Functor)

newtype State = State { getIO :: IO () }
type CoDSL a = Cofree CoDSLF a

-- Monad transformers may be better here in practise, but this is a prototype anyways
next :: State -> CoDSLF State
next s = CoDSLF (coPyFn s) (undefined s)

coPyFn :: State -> String -> DirRef -> (PyMod, State)
coPyFn s st ref = (mod, undefined)
  where
    mod = PyMod st
    computation = print "ref"
    
