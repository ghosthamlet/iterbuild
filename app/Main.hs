{-# LANGUAGE ApplicativeDo #-}

module Main where

import Expr
import Data.Typeable


-- lets define some nonsense - this definetly looks better than the code I had without do notation. Here the structure of the program is instantly clear by
-- looking at the last line and while it now occupies many more lines than earlier, the following simple rules of thumb emerge:
-- 1) Introducing new objects that have not been defined in my DSL will require you to introduce them in an either named or unnamed way as a new 'variable'.
-- Note that new objects can and probably should be defined outside of the actual do block, so they are reusable.
-- 2) Introducing objects I defined in my own library should only require you to just use the functions and store the result with arrow notation
-- More fine tuned ideas(like auto-naming of function parameters that can be refered to by specifying the function and then the param name)
-- should probably be only be implemented after I have written some code in this language. There are probably ways to avoid boilerplate, because
-- quick iteration is the name of the game here.
finalString :: Expr String
finalString = do
        -- 1) introduce all named variables
        (+) <- label "operator" (+)
        cosmc <- label "cosmc" (3 :: Int)
        birthday <- label "birthday" 77
        key <- label "api key" "2379ca13f24ae"

        -- 2) optionally introduce other operations
        return $ (++) key (show (cosmc + birthday))

-- compute the expression
out = collapse finalString

-- lets replace the cosmological constant, the api key and the mystery operation
replacer :: Typeable a => Labeled a -> Labeled a
replacer = mapKey "cosmc" (+(1 :: Int)) -- add one to the cosmological constant
    `also` replaceKey "api key" "23af23e3e4bc"
    `also` replaceKey "operator" ((*) :: Int -> Int -> Int)
    `also` id

replacedExpr = replace replacer finalString
out' = collapse replacedExpr


main :: IO ()
main = putStrLn out >> putStrLn out'

