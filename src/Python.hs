{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Python where

import GHC.Generics
import Text.Printf
import Data.String
import Data.Hashable
import Control.Monad.Reader
import Control.Applicative
import Capabilities
import Expr

-- The string is interpreted as a module name relative to the bin path
newtype PyModule = PyModule { pyModule :: FilePath } deriving (Hashable)

instance IsString PyModule where
    fromString = PyModule

data PyObject = PyObject { location :: PyModule, identifier :: String } deriving (Generic)

instance Hashable PyObject where
    hash obj = hash (location obj, identifier obj)

genLines :: PyObject -> [PyObject] -> [(String, PyObject)] -> [String]
genLines obj args kwargs  =
    let paramString =
            foldl (printf "%s, %s") "" $
                (identifier <$> args) ++ ((\(k,v) -> printf "%s=%s" k (identifier v)) <$> kwargs)
    in let
        objects = args ++ (snd <$> kwargs)
    in
        -- TODO: the location should be tweaked to make some arbitrary path relavive to target and then
        -- use a general python-internal function that can import from any path.
        ((\obj -> printf "from %s import %s" (pyModule $ location obj) (identifier obj)) <$> objects)
            ++ [
                "def run(data):",
                printf "    return %s(%s)" (identifier obj) paramString
            ]

joinLines :: [String] -> String
joinLines = foldl (printf "%s\n%s") ""

namedApply :: (Applicative f, HasFileNamer f, HasTargetWriter f) => f PyObject -> f String -> f [PyObject] -> f [(String, PyObject)] -> f PyObject
namedApply obj name args kwargs =
    let
        code = joinLines <$> (genLines <$> obj <*> args <*> kwargs)
    in let
        fileName = nameFile obj name
    in
        writeTarget fileName code
        *> ((\fname -> PyObject { location=PyModule fname, identifier="run" }) <$> fileName)
    -- A do block is probably much worse than this reasonably concise code. Moreover applicative do does not
    -- allow you to do some things where you think that you would not need monads, but it insists it does.
    -- One can probably write non-effectfull functions easily for anything that implements 'String'/'Num'/'Bool' typeclasses,
    -- so that one always has functions of the form f a1 -> ... -> f an by default.

apply :: (Applicative f, HasFileNamer f, HasTargetWriter f) => f PyObject -> f [PyObject] -> f [(String, PyObject)] -> f PyObject
apply obj = namedApply obj (pure "")

-- compose :: (Applicative f, HasFileNamer f, HasTargetWriter f) => f [PyObject] -> PyObject
