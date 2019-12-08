{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Python where

import Data.String
import Data.Hashable
import Capabilities
import Expr

-- The string is interpreted as a module name relative to the bin path
newtype PyModule = PyModule { pyModule :: FilePath } deriving (Hashable)

instance IsString PyModule where
    fromString = PyModule

data PyObject = PyObject { location :: PyModule, identifier :: String }

namedApply :: (Monad m, HasTargetWriter m) => ExprM m PyObject -> ExprM m (String -> [PyObject] -> [(String, PyObject)] -> PyObject)
namedApply obj =
    let paramString args kwargs =
            foldl (\p1 p2 -> p1 ++ ", " ++ p2) "" $
                (identifier <$> args) ++ ((\(k,v) -> k ++ "=" ++ identifier v) <$> kwargs)
    in let
        objects args kwargs = args ++ (snd <$> kwargs)
    in
        undefined
        -- nameFile obj