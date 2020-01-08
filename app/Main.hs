{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude hiding ((%))
import Formatting

import Options.Applicative
import Data.Semigroup ((<>))

import Capabilities


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
