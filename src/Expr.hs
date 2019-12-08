{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Typeable
import Data.Generics.Aliases

-- we need our own free applicative because we want to quantify over Typeable, since otherwise the
-- compiler enforces parametricity.
data FreeA f a where
    Pure :: a -> FreeA f a
    Ap   :: Typeable a => f a -> FreeA f (a -> b) -> FreeA f b

instance Functor (FreeA f) where
    fmap f (Pure a)   = Pure (f a)
    fmap f (Ap x y)   = Ap x ((f .) <$> y)
    
instance Applicative (FreeA f) where
    pure = Pure
    Pure f <*> y = fmap f y
    Ap x y <*> z = Ap x (flip <$> y <*> z)

runAp :: Applicative g => (forall x. Typeable x => f x -> g x) -> FreeA f a -> g a
runAp _ (Pure x) = pure x
runAp u (Ap f x) = flip id <$> u f <*> runAp u x

-- | Tear down a free 'Applicative' using iteration.
-- Map contents around until you hit the target element in g a. Then apply the input function.
iterAp :: Functor g => (g a -> a) -> FreeA g a -> a
iterAp algebra = go
    where go (Pure a) = a
          go (Ap underlying apply) = algebra (go . (apply <*>) . pure <$> underlying)

replace :: (forall a. Typeable a => f a -> g a) -> FreeA f b -> FreeA g b
replace _ (Pure a) = Pure a
replace f (Ap x y) = Ap (f x) (replace f y)


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

type Expr = FreeA Labeled

-- we don't want to really use the regular "Pure" since you can't replace whatever you put in there, so this is the solution
expr :: Typeable a => a -> Expr a
expr x = Ap (return x) (Pure id)

-- computes the value of the expression
collapse :: Expr a -> a
collapse = iterAp forget

-- compose replacers, on the right comes the generic replacer and on the left the specific replacer.
also :: (Typeable a, Typeable b) => (b -> b) -> (a -> a) -> (a -> a)
also f g = extT g f
infixr 7 `also`

forget (Anon x) = x
forget (Labeled _ x) = x

label :: Typeable a => String -> a -> Expr a
label s x = Ap (Labeled s x) (Pure id)

-- replace the elements with the correct label
replaceKey :: String -> (a -> a) -> Labeled a -> Labeled a
replaceKey key f (Anon x) = Anon x
replaceKey key f (Labeled key' value) = if key == key'
    then Labeled key' $ f value
    else Labeled key' value

-- Note that for now you can't directly replace a sub-expression, for doing that you need to pattern match directly on the free
-- applicative, I might write a combinator for that at some point.
