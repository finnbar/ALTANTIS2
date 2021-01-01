module Main where

import ALTANTIS2 (doDiscord, makeGameState)

import Discord
import Discord.Types

import LoadEnv
import System.Environment (lookupEnv)
import Data.Default
import Data.Text (pack)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    loadEnv
    token <- pack . fromMaybe def <$> lookupEnv "DISCORD_TOKEN"
    gs <- makeGameState
    doDiscord token gs