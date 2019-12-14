{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Python where

import Protolude hiding (second, (%))
import GHC.Generics
import Control.Arrow
import Data.List(foldl)
import Data.Text.Lazy(unlines)
import Path
import System.FilePath
import Formatting

import Capabilities
import Expr


-- The string is interpreted as a module name relative to the bin path
newtype PyModule = PyModule { pyModule :: Path Rel File }

toImportPath :: PyModule -> LText
toImportPath = foldl (format (text % "." % text)) "" . fmap toS . splitDirectories . toFilePath . pyModule

data PyObject = PyObject { location :: PyModule, identifier :: LText } deriving (Generic)


genLines :: PyObject -> [PyObject] -> [(LText, PyObject)] -> [LText]
genLines obj args kwargs  =
    let paramString =
            foldl (format (text % ", " % text)) "" $
                (identifier <$> args) ++ (identifier . snd <$> kwargs)
    in let
        objects = args ++ (snd <$> kwargs) ++ [obj]
    in
        (uncurry (format ("from " % text % " import " % text))
                . (toImportPath . location &&& identifier)
                <$> objects)
        ++ [
            "def run(data):",
            format ("    return " % text % "(" % text % ")") (identifier obj) paramString
        ]

-- args: file label, function name, contents
makeObject :: (Applicative f, HasTargetPath f) => f (Maybe LText) -> f LText -> f LText -> f PyObject
makeObject label funName = (liftA2 (flip (\fPath -> PyObject (PyModule fPath))) funName) . addContent label

-- This should probably be written as a polyvariadic function, like printf "%s".
apply_ :: (Applicative f, HasTargetPath f) => f (Maybe LText) -> f PyObject -> f [PyObject] -> f [(LText, PyObject)] -> f PyObject
apply_ label = (((makeObject label (pure "run") . fmap unlines).).) . liftE3 genLines


apply :: (Applicative f, HasTargetPath f) => f PyObject -> f [PyObject] -> f [(LText, PyObject)] -> f PyObject
apply = apply_ (pure Nothing)

labelApply :: (Applicative f, HasTargetPath f) => f LText -> f PyObject -> f [PyObject] -> f [(LText, PyObject)] -> f PyObject
labelApply l = apply_ (Just <$> l)

composition :: (Applicative f, HasTargetPath f) => f PyObject
composition =
    let
        code = unlines [
            "def compose(functions):",
            "    if functions == []:",
            "        return lambda x: x",
            "    else:",
            "        return lambda x: compose(functions[1:])(functions[0](x))" ]
    in
        -- I don't thing anyone would ever want a human-readable name for this
        makeObject (pure Nothing) (pure "compose") (pure code)
