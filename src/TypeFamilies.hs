{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- why the fuck is this not on by default??
module TypeFamilies where

import Data.Typeable (cast, Typeable)
import Data.Generics.Aliases

data Labeled a =
    Anon a
    | Labeled String a deriving (Functor, Typeable)

forget (Anon x) = x
forget (Labeled _ x) = x

instance Applicative Labeled where
    pure = Anon
    f <*> z = Anon (forget f $ forget z)

-- we need our own applicative because we want to quantify over Typeable.
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
iterAp :: Functor g => (g a -> a) -> FreeA g a -> a
iterAp algebra = go
  where go (Pure a) = a
        go (Ap underlying apply) = algebra (go . (apply <*>) . pure <$> underlying)

hoistAp :: (forall a. f a -> g a) -> FreeA f b -> FreeA g b
hoistAp _ (Pure a) = Pure a
hoistAp f (Ap x y) = Ap (f x) (hoistAp f y)

type Expr = FreeA Labeled

-- we don't want to really use the regular "Pure" since you can't replace whatever you put in there, so this is the solution
expr :: Typeable a => a -> Expr a
expr x = Ap (Anon x) (Pure id)

name :: Typeable a => String -> a -> Expr a
name s x = Ap (Labeled s x) (Pure id)

-- The following does not typecheck:
-- replace :: Typeable a => a -> a
-- replace x = case (cast x :: Maybe Int) of
--                 Just y -> 5
--                 Nothing -> x
-- ..so you really can't cheat this blatantly. The output type cannot be generic in a. It suprises me that haskell can't upcast, but I
-- guess this is part of parametericity. Thus the only place where you really can replace, is right before execution in some kind of interpreter
-- that gets some options of what to replace with what. Which is fine I guess, I don't see a reason why this should constrain me, I have taken
-- this too far anyways and I want to get it done.

fn :: (Typeable a) => Expr a -> Expr String
fn x = case (cast x :: Maybe (Expr (Int -> String))) of
        Just f -> f <*> expr 0
        Nothing -> Pure "no comment"

-- At least this works. This shall suffice. But can I compose multiple replacements? Composition probably has to happen before interpretation.
-- Let's write some toy code.

secretIngredient :: Expr String
secretIngredient = expr show <*> (name "mystery operation" (+) <*> name "cosmological constant" (3 :: Int) <*> name "my birthday" 77)

phrase = expr (++) <*> secretIngredient <*> name "api key" "adhfypvq[ao"

-- Probably one could also auto-generate the names with a GUID/Random monad or hashes(there is a good looking "hashable" package) or sth.
-- For now I will procrastinate that.

-- iterAp maps the values around inside "Labeled" and at the end applies forget to get them out.
out = iterAp forget phrase

-- lets replace the cosmological constant
incrIfInt :: Typeable a => a -> a
incrIfInt = mkT ((+100) :: Int -> Int)

replace :: Typeable a => Labeled a -> Labeled a
replace (Labeled "cosmological constant" value) = Labeled "cosmolocial constant" $ incrIfInt value
replace x = x

out' = forget $ runAp replace phrase
