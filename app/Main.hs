{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
-- Extended default rules is quite nice because it uses the required typeable constraint
-- required by 'label' and 'also', and casts everything down to a default like String or
-- Int. Of course, is sucks that I can't use LText, but that could theoretically be arranged
-- by constraining the Expression types even further and not include String.
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Protolude hiding ((%))
import Data.Typeable
import Data.String
import Formatting

import Options.Applicative
import Data.Semigroup ((<>))

import Expr
import Capabilities

-- lets define some nonsense - this definetly looks better than the code I had without do notation. Here the structure of the program is instantly clear by
-- looking at the last line and while it now occupies many more lines than earlier, the following simple rules of thumb emerge:
-- 1) Introducing new objects that have not been defined in my DSL will require you to introduce them in an either named or unnamed way as a new 'variable'.
-- Note that new objects can and probably should be defined outside of the actual do block, so they are reusable.
-- 2) Introducing objects I defined in my own library should only require you to just use the functions and store the result with arrow notation
-- More fine tuned ideas(like auto-naming of function parameters that can be refered to by specifying the function and then the param name)
-- should probably be only be implemented after I have written some code in this language. There are probably ways to avoid boilerplate, because
-- quick iteration is the name of the game here.

-- Inspecting an expression's tree and giving the user hashes for each replaceable component in a command line utility, might be a good idea. Because
-- our goal is to reduce as much explicit variable naming. The other option is that the developer goes back to previous code and modifies the variable
-- names, and then iterbuild can check whether the code is the same as the one for an old saved run by computing a hash.
finalString :: Expr String
finalString = do
        -- 1) introduce all named variables
        (+) <- label "operator" (+)
        cosmc <- label "cosmc" 3
        birthday <- label "birthday" 77
        key <- label "api key" "2379ca13f24ae"

        -- 2) optionally introduce other operations
        return $ (<>) key (show (cosmc + birthday))

-- compute the expression
out = collapse finalString

replacer :: Typeable a => Labeled a -> Labeled a
replacer = mapKey "cosmc" (+1) -- add one to the cosmological constant
    `also` replaceKey "api key" "23af23e3e4bc"
    `also` replaceKey "operator" (*)
    `also` identity

replacedExpr = replaceE replacer finalString
out' = collapse replacedExpr

data PrefetchInput =
    Competition LText
    | URL LText

newtype Input = PrefetchInput PrefetchInput

competitionInput :: Parser PrefetchInput
competitionInput = Competition <$> strOption (long "kaggle" <> metavar "COMPETITION" <> help "Kaggle competition name, as accepted by the kaggle CLI.")

urlInput :: Parser PrefetchInput
urlInput = URL <$> strOption (long "url" <> metavar "URL" <> help "Url you want to prefetch. (NOT IMPLEMENTED YET)")

prefetchOptions :: Parser Input
prefetchOptions = PrefetchInput <$> (competitionInput <|> urlInput)

parser = hsubparser (
        command "prefetch" (info prefetchOptions (progDesc "Fetches content from the specified location and puts it into the cache, printing the location where it was stored. The location contains the hash of the file/folder. If as a user of iterbuild, you want to use files/folders from remote places, you are required to provide the hash of that content(eg. in the fetchKaggle function). This hash is adquired by running this command. Note the once the hash is specified, this command is never needed again, since the functions that fetch from a url will download the content automatically and verify the hash."))
    )

opts :: ParserInfo Input
opts = info (parser <**> helper) (fullDesc <> header "iterbuild - command line tools for managing iterbuild projects." <> progDesc "For now just contains utilities for importing data from external URLs.")

parseInput :: Input -> IO ()
parseInput (PrefetchInput (Competition comp)) = 
    (withDefaults $ format ("The resulting hash is: " % text) . show . fst <$> fetchKaggle_ comp) >>= putStrLn

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) opts >>= parseInput
