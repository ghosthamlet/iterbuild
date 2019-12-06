
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

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

class Monad f => Wrapped f where
    forget :: f a -> a
    -- We need: `forget . return = id`, `fmap forget = join`

-- we don't want to really use the regular "Pure" since you can't replace whatever you put in there, so this is the solution
expr :: (Wrapped f, Typeable a) => a -> FreeA f a
expr x = Ap (return x) (Pure id)

-- computes the value of the expression
collapse :: (Functor f, Wrapped f) => FreeA f a -> a
collapse = iterAp forget

-- compose replacers, on the right comes the generic replacer and on the left the specific replacer.
also :: (Typeable a, Typeable b) => (b -> b) -> (a -> a) -> (a -> a)
also f g = extT g f
infixr 7 `also`

-- Example usage(Int anotation is necessary since otherwise 1 is a Num.):
-- replace :: Typeable a => a -> a
-- replace = (+(1 :: Int)) `also` (++"hello") `also` id

-- -- This exists so type inference works
-- fmapM :: (a -> a) -> Maybe a -> Maybe a
-- fmapM = fmap

-- replaceM :: Typeable a => Maybe a -> Maybe a
-- replaceM = fmapM (+(1 :: Int)) `also` fmapM (++"hello") `also` id

