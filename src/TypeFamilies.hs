{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- why the fuck is this not on by default??
module TypeFamilies where

import Control.Applicative.Free
import Data.Functor.Coyoneda

class GMapKey k where
    data GMap k :: * -> *
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v

data Hideable a = a ~ Int  => IntHideable a | a ~ String => StringHideable a

-- lol, this has no constructor, so correct pattern matching is possible at a later point
newtype NonExistant = Hideable Char

-- Note that we can use existential quantification to completely remove the type signature,
-- though this is not what we want since free applicative will do this for us
data AnyHideable = forall a. AnyHideable (Hideable a)

incHideable :: Hideable Int -> Int
incHideable (IntHideable x) = x + 1

foo :: AnyHideable -> Int
foo (AnyHideable x) = case x of
    IntHideable {} -> incHideable x
    StringHideable {} -> 0

a = foo (AnyHideable (StringHideable "hello"))

-- Ok, now lets try to write a natural transformation that matches on type. We will perform this on top of Hideable.
-- Let's "replace" only the integers with their increment.
increment :: Hideable Int -> Hideable Int
increment (IntHideable i) = IntHideable (i + 1)

eta :: Hideable a -> Hideable a
eta x = case x of
    IntHideable {} -> increment x
    StringHideable {} -> x

-- Okay, this is seriously insane. I think this is everything I ever wanted. Now we will want to not only operate
-- on my "basic data types" string and int, but also on stuffs like String -> Int. Currently, Hideable (String -> Int) has
-- absolutely no constructors, so it is the empty type. Thus the free applicative over hideable is not very useful.

-- Make generic over any family of base types?
data ExprGADT a where
    GInt :: Int -> ExprGADT Int
    GString :: String -> ExprGADT String
    GFun :: (ExprGADT b -> ExprGADT c) -> ExprGADT (b -> c)
    GProd :: ExprGADT b -> ExprGADT c -> ExprGADT (b, c)
    GCoProd :: ExprGADT b -> ExprGADT c -> ExprGADT (Either b c)

-- type Expr2 a = Coyoneda ExprGADT a


-- This is absolutely not functorial, since you can't even lift a Int -> Float since Float lifts to the empty type.
-- One solution might be to erase the type parameter with existential types - this causes the free applicative to not
-- be used on arbitrary types, but only on one type. Thus we will just have a family of functions from one type to
-- itself and we would loose static type checking.
-- Now we can apply the Yoneda trick:
data Expr z =
    -- look at the type of the constructor - its output truly is the natural transformation corresponding to
    -- (ExprGADT Int) under the yoneda isomorphism. This is basically a constructor that is isomorphic to the
    -- old constructor. To not get confused, one should always just ignore the last argument and assume it will
    -- be the identity map (as in the yoneda isomorphism).
    EInt Int (Int -> z)
    | EString String (String -> z)
    | forall b c. EFun (Expr b -> Expr c) ((b -> c) -> z)
    | forall b c. EProd (Expr b, Expr c) ((b, c) -> z)
    | forall b c. ECoProd (Either (Expr b) (Expr c)) (Either b c -> z)

-- Yes, Expr is a functor, but this is not magic. It is not a functor in the natural transformations that would correspond to
-- objects in ExprGADT, but rather functorial in the mysterious argument z, which is an artefact of currying into a natural transformation.
-- Thus the functoriality of Expr can't be transfered onto GADTExpr. On a more down-to-earth level, we made ExprGADT into a functor
-- by adding just parameterizing additionally on a "what to do next" argument, so the functor instance can now be cannonically defined
-- by cheating:
instance Functor Expr where
    fmap f (EInt x i) = EInt x (f . i)
    fmap f (EString x i) = EString x (f . i)
    fmap f (EFun x i) = EFun x (f . i)
    fmap f (EProd x i) = EProd x (f . i)
    fmap f (ECoProd x i) = ECoProd x (f . i)

-- But our cheating does not really have negative effects, since we will avoid mapping on Expr directly - we want to use a free applicative
-- anyways and mapping would only ocurr on rare cases.
fw :: (Functor f) => (forall b . (a -> b) -> f b) -> f a
fw f = f id

bw :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
bw x f = fmap f x

-- Now we can easily get the old constructors by conststructing toghether with the identity map
eint n = fw (EInt n)
estring s = fw (EString s)
efun :: (Expr b -> Expr c) -> Expr (b -> c)
efun f = fw (EFun f)
eprod :: (Expr b, Expr c) -> Expr (b, c)
eprod x = fw (EProd x)
ecoprod :: Either (Expr b) (Expr c) -> Expr (Either b c)
ecoprod x = fw (ECoProd x)

-- Now lets start the real fun! Replaceable expressions!
type RExpr a = Ap Expr a
-- evaluation should always be our start type, since the start type is not replaceable

incr :: Expr Int -> Expr Int
-- the nonexhaustive match is because the input of type Expr Int could literally constructed in any way -
-- as long as afterwards a function z -> Int was slapped on top.
incr (EInt n i) = EInt (n+1) i
decr :: Expr Int -> Expr Int
decr (EInt n i) = EInt (n-1) i

-- note that replacement will not work on "Pure id", since it is not an Expr. We can only apply natural transformations
-- forall z. Expr z -> Expr z
expr = Ap (eint 5) (Ap (efun incr) (Pure id))
-- now we can easily replace incr by decr.
replace :: Expr a -> Expr a
replace (EFun f i) = undefined -- How can I specifically match on the constructor of f with type parameters Int, Int?


-- data Expr' z =
--     EInt' Int (Int -> z)
--     | EString String (String -> z)
--     | forall b c. EFun (Expr b -> Expr c) ((b -> c) -> z)
--     | forall b c. EProd (Expr b, Expr c) ((b, c) -> z)
--     | forall b c. ECoProd (Either (Expr b) (Expr c)) (Either b c -> z)