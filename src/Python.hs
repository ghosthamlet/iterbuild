{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Python where

import Protolude hiding (second, (%))
import Control.Arrow
import Data.List(foldl)
import Data.Text.Lazy(unlines)
import Path hiding ((</>))
import System.FilePath
import Formatting

import Capabilities
import Expr


-- The string is interpreted as a module name relative to the bin path
newtype PyModule = PyModule { pyModule :: Path Rel File } deriving (Hashable)

toImportPath :: PyModule -> LText
toImportPath = foldl (format (text % "." % text)) "" . fmap toS . splitDirectories . toFilePath . pyModule

data PyObject = PyObject { location :: PyModule, identifier :: LText } deriving (Generic)

instance Hashable PyObject where
    hash obj = hash (location obj, identifier obj)

applyCode :: PyObject -> [PyObject] -> [(LText, PyObject)] -> [LText]
applyCode obj args kwargs  =
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
    

cachedApplyCode :: Path Abs Dir -> PyObject -> [PyObject] -> [(LText, PyObject)] -> [LText]
cachedApplyCode cachePath obj args kwargs  =
    let paramString =
            foldl (format (text % ", " % text)) "" $
                (identifier <$> args) ++ (identifier . snd <$> kwargs)
    in let
        objects = args ++ (snd <$> kwargs) ++ [obj]
    in let
        objCachePath = toFilePath cachePath </> (toS . format (base 36) . hash . concatMap (toFilePath . pyModule . location) $ objects)
    in
        (uncurry (format ("from " % text % " import " % text))
                . (toImportPath . location &&& identifier)
                <$> objects)
        ++ ["import os.path",
            format ("   if os.path.isfile('" % string % "'):") objCachePath,
            "else:",
            format ("       result = " % text % "(" % text % ")") (identifier obj) paramString,
            "       # TODO: write to cache"]

-- args: file label, function name, contents
makeObject :: (Applicative f, HasTargetPath f) => f (Maybe LText) -> f LText -> f LText -> f PyObject
makeObject label funName = liftA2 (flip (PyObject . PyModule)) funName . addContent label (pure . Just $ "py") . fmap toS

-- This should probably be written as a polyvariadic function, like printf "%s".
apply :: (Applicative f, HasTargetPath f) => f PyObject -> f [PyObject] -> f [(LText, PyObject)] -> f PyObject
apply = (((makeObject (pure . Just $ "apply") (pure "run") . fmap unlines).).) . liftE3 applyCode


compose :: (Applicative f, HasTargetPath f) => f PyObject
compose =
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

-- The input function is probably a function which contains references to other modules,
-- but since those modules have hashes in their names :) and the function name is a unique
-- identifier for the function(since it contains a hash of its own code), I can just lookup
-- the ha
-- cacheFn :: Path Abs Dir -> PyObject -> PyObject
-- cacheFn cachePath obj =
--     let
--         code = unlines [
--             "import os.path",
--             "def wrap(f):",
--             format ("   if os.path.isfile('" % text % "'):") (cachePath </> hash obj),
--             "" ]
--     in
--         makeObject (pure Nothing) (pure "wrap") (code . toS . toFilePath <$> cachePath)