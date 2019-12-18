-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where


import Protolude

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

-- pr :: Name -> ExpQ
-- pr n = [| putStrLn ( $(lift (nameBase n ++ " = ")) ++ show $(varE n) ) |]

-- a = "hello"
-- main = $(pr 'a)

-- class Num a => IntSemigroup a where
--     comp :: a -> a -> a

class Semigroup' a where
    comp :: a -> a -> a

class Semigroup' a => Monoid' a where
    id :: a

-- instance Num a => Semigroup' a where
--     comp = (+)

instance Semigroup' LText where
    comp = (<>)

instance (Semigroup' a, Num a) => Monoid' a where
    id = 0