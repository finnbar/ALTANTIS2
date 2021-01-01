module Main where

import Lib

import Discord
import Discord.Types

import LoadEnv
import System.Environment (lookupEnv)

import qualified Data.Text.IO as TIO (putStrLn)
import Data.Text (pack)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    loadEnv
    token <- pack . (fromMaybe "") <$> lookupEnv "DISCORD_TOKEN"
    userFacingError <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler
        }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler = const $ pure ()