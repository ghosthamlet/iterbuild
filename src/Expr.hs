{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Typeable
import Data.Generics.Aliases
import Data.Functor.Compose

-- we need our own free applicative because we want to quantify over Typeable, since otherwise the
-- compiler enforces parametricity.
data FreeA f a where
    Pure :: a -> FreeA f a
    Ap   :: Typeable a => f a -> FreeA f (a -> b) -> FreeA f b

instance Functor (FreeA f) where
    -- This is essentially post composition but in confusing because our functions are curried, so in order to post
    -- compose to a curried function, we need to iterate the post-composition operation n times.
    fmap f (Pure a)   = Pure (f a)
    fmap f (Ap x y)   = Ap x ((f .) <$> y)
    
instance Applicative (FreeA f) where
    pure = Pure
    -- Essentially <*> flips the arguments of the innermost "pure" function such that one can apply map.
    -- We use the innermost pure function of the first argument and the innermost pure function of the second argument
    -- in order to generate a cannonical new innermost function. So if we use the identity in the inner functions, <*>
    -- will just generate permutation functions as innermost functions and compose only with <*>.
    Pure f <*> y = fmap f y
    Ap x y <*> z = Ap x (flip <$> y <*> z)

-- we don't want to really use the regular "Pure" since you can't replace whatever you put in there, so this is the solution
-- Interestingly, this lifting is the key point where we need the typeably constraint.
expr :: Typeable a => a -> Expr a
expr x = Ap (return x) (Pure id)

-- We want to lift functions like follows, in order to apply the arguments only "formally" and preserve the expression structure.
liftE :: Applicative f => (a -> b) -> f a -> f b
liftE f x = pure f <*> x

liftE2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftE2 f x y = liftE f x <*> y

liftE3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftE3 f x y z = liftE2 f x y <*> z

liftE4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftE4 f x y z a = liftE3 f x y z <*> a
-- one may be able to build a polyvariadic function, so the user does not have to know the number of arguments (this code is
-- just plain retarded anyways)

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

instance Monad Labeled where
    return = Anon
    x >>= f = forget (fmap f x)

type Expr = FreeA Labeled

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

-- map the elements with the correct label
mapKey :: String -> (a -> a) -> Labeled a -> Labeled a
mapKey key f (Anon x) = Anon x
mapKey key f (Labeled key' value) = if key == key'
    then Labeled key' $ f value
    else Labeled key' value

replaceKey :: String -> a -> Labeled a -> Labeled a
replaceKey s = mapKey s . const

-- Note that for now you can't directly replace a sub-expression, for doing that you need to pattern match directly on the free
-- applicative, I might write a combinator for that at some point.

-- Here are reasonable combinators for when dealing with precomposed monads
-- We use map here, so we need to wait for the functor to 'compute' (think of it as, say, an IO monad)
mapKeyLift :: Functor f => String -> (a -> a) -> Compose Labeled f a -> Compose Labeled f a
mapKeyLift s = (Compose .) . (. getCompose) . mapKey s . fmap

-- This just trashes the previous computation by inserting a new value with pure
replaceKeyLift :: Applicative f => String -> a -> Compose Labeled f a -> Compose Labeled f a
replaceKeyLift s = (Compose .) . (. getCompose) . replaceKey s . pure

-- Utilities
-- instance (Functor f, Num a) => Num (f a) where