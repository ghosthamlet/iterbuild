{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Protolude
import Data.Generics.Aliases
import Data.Functor.Compose

-- | This is a variant of the <http://hackage.haskell.org/package/free-5.1.3/docs/Control-Applicative-Free.html Free Applicative>.
-- The difference is that we need a Typeable type constraint in Apply, so we can later change the f a argument of Apply for only one
-- specified type.
data FreeA f a where
    Pure :: a -> FreeA f a
    Apply :: Typeable a => f a -> FreeA f (a -> b) -> FreeA f b

instance Functor (FreeA f) where
    -- | This is essentially post composition but in confusing because our functions are curried, so in order to post
    -- compose to a curried function, we need to iterate the post-composition operation n times.
    fmap f (Pure a)   = Pure (f a)
    fmap f (Apply x y)   = Apply x ((f .) <$> y)
    
instance Applicative (FreeA f) where
    pure = Pure
    -- | Essentially <*> flips the arguments of the innermost "pure" function such that one can apply map.
    -- We use the innermost pure function of the first argument and the innermost pure function of the second argument
    -- in order to generate a cannonical new innermost function. So if we use the identity in the inner functions, <*>
    -- will just generate permutation functions as innermost functions and compose only with <*>.
    Pure f <*> y = fmap f y
    Apply x y <*> z = Apply x (flip <$> y <*> z)

-- | Lifts a value into an 'Expr' such that the value can be replaced by a different value later.
expr :: Typeable a => a -> Expr a
expr x = Apply (return x) (Pure identity)

-- | Creates a labeled expression.
label :: Typeable a => LText -> a -> Expr a
label s x = Apply (Labeled s x) (Pure identity)

-- | Lifts a function while ensuring the function itself is replaceable later.
liftE :: Applicative f => (a -> b) -> f a -> f b
liftE f x = pure f <*> x

-- | Lifts a function of 2 arguments while ensuring the function itself is replaceable later.
liftE2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftE2 f x y = liftE f x <*> y

liftE3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftE3 f x y z = liftE2 f x y <*> z

liftE4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftE4 f x y z a = liftE3 f x y z <*> a
-- TODO: one may be able to build a polyvariadic function, so the user does not have to know the number of arguments (this code is
-- just plain retarded anyways)

-- | The isomorphism of the adjunction.
runAp :: Applicative g => (forall x. Typeable x => f x -> g x) -> FreeA f a -> g a
runAp _ (Pure x) = pure x
runAp u (Apply f x) = flip identity <$> u f <*> runAp u x

-- | Tear down a 'FreeA' using iteration.
iterAp :: Functor g => (g a -> a) -> FreeA g a -> a
iterAp algebra = go
    where go (Pure a) = a
          go (Apply underlying apply) = algebra (go . (apply <*>) . pure <$> underlying)

-- | This is the key function for doing replacement.
replaceE :: (forall a. Typeable a => f a -> g a) -> FreeA f b -> FreeA g b
replaceE _ (Pure a) = Pure a
replaceE f (Apply x y) = Apply (f x) (replaceE f y)

-- | Base functor used for defining 'Expr'.
data Labeled a =
    Anon a
    | Labeled LText a deriving (Functor, Typeable)

-- | Extract the labeled value.
forget (Anon x) = x
forget (Labeled _ x) = x

instance Applicative Labeled where
    pure = Anon
    f <*> z = Anon (forget f $ forget z)

instance Monad Labeled where
    return = Anon
    x >>= f = forget (fmap f x)

-- | The main applicative functor. As long as values are of this type and they are constructed with the correct lifting
-- operations ('expr','liftE',...), you will be able to replace them later. This functor will later be composed with
-- a monad to generate an applicative that can actually do effects.
type Expr = FreeA Labeled

-- | Compute the value of the expression.
collapse :: Expr a -> a
collapse = iterAp forget

-- | A combinator for composing replacers. First argument is the generic replacer and the second one the specific replacer.
-- For example, given @ f :: Labeled Int -> Labeled Int @ and @ g :: Labeled Text -> Labeled Text @, you can map the values in the
-- expression of type `Labeled Int` with `f` and the ones of type `Labeled Text` with `g`, by running:
--
-- @
--      replaceE (f `also` g `also` id) yourExpression
-- @
--
-- Note that `f` and `g` have the capability to look at variable names, for instance we can specify such `f` and `g` with 'mapKey' and
-- 'replaceKey'.
also :: (Typeable a, Typeable b) => (b -> b) -> (a -> a) -> (a -> a)
also f g = extT g f
infixr 7 `also`

-- | Map the elements matching the label.
mapKey :: LText -> (a -> a) -> Labeled a -> Labeled a
mapKey key f (Anon x) = Anon x
mapKey key f (Labeled key' value) = if key == key'
    then Labeled key' $ f value
    else Labeled key' value

replaceKey :: LText -> a -> Labeled a -> Labeled a
replaceKey s = mapKey s . const

-- TODO: Note that for now you can't directly replace a sub-expression, for doing that you need to pattern match directly on the free
-- applicative, I might write a combinator for that at some point.

-- Here are reasonable combinators for when dealing with precomposed monads
-- We use map here, so we need to wait for the functor to 'compute' (think of it as, say, an IO monad)
mapKeyLift :: Functor f => LText -> (a -> a) -> Compose Labeled f a -> Compose Labeled f a
mapKeyLift s = (Compose .) . (. getCompose) . mapKey s . fmap

-- | Trashes the previous computation by inserting a new value with pure
replaceKeyLift :: Applicative f => LText -> a -> Compose Labeled f a -> Compose Labeled f a
replaceKeyLift s = (Compose .) . (. getCompose) . replaceKey s . pure
