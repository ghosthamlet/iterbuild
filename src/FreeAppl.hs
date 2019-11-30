{-# LANGUAGE DeriveFunctor         #-}
module FreeAppl where

import Control.Applicative.Free

data User = User { username :: String, fullname :: String, ident :: Int } deriving (Show)

data Option a = Option { optName :: String, optDefault :: Maybe a, optReader :: String -> Maybe a} deriving (Functor)

one :: Option a -> Ap Option a
one x = Ap x (Pure id)
-- userP :: Ap Option (Maybe String)
-- userP = undefined 

a = User
    <$> one (Option "username" Nothing Just)
    <*> one (Option "fullname" (Just "") Just)
    <*> one (Option "id" Nothing undefined)

parserDefault :: Ap Option a -> Maybe a
parserDefault (Pure x) = Just x
parserDefault (Ap opt f) = parserDefault f <*> optDefault opt
