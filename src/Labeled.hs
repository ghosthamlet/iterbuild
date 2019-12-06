{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- why the fuck is this not on by default??
module Labeled where

import Expr
import Data.Typeable

data Labeled a =
    Anon a
    | Labeled String a deriving (Functor, Typeable)

instance Applicative Labeled where
    pure = Anon
    f <*> z = Anon (forget f $ forget z)

-- lultz, I can use forget - lazyness for the win.
instance Monad Labeled where
    return = Anon
    x >>= f = forget (fmap f x)

instance Wrapped Labeled where
    forget (Anon x) = x
    forget (Labeled _ x) = x

type Expr = FreeA Labeled

label :: Typeable a => String -> a -> Expr a
label s x = Ap (Labeled s x) (Pure id)

-- replace the elements with the correct label
replaceKey :: String -> (a -> a) -> Labeled a -> Labeled a
replaceKey key f (Anon x) = Anon x
replaceKey key f (Labeled key' value) = if key == key'
    then Labeled key' $ f value
    else Labeled key' value

-- At least this works. This shall suffice. But can I compose multiple replacements? Composition probably has to happen before interpretation.
-- Let's write some toy code. The expression specification api looks quite compact.

-- Probably one could also auto-generate the names with a GUID/Random monad or hashes(there is a good looking "hashable" package) or sth.
-- For now I will procrastinate that.
-- Also note that for now you can't directly replace a sub-expression, for doing that you need to pattern match directly on the free
-- applicative, I might write a combinator for that at some point.
