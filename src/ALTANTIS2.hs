module ALTANTIS2
    ( doDiscord
    ) where

import Discord
import Discord.Types

import Data.Text

import qualified Data.Text.IO as TIO (putStrLn)

doDiscord :: Text -> IO ()
doDiscord token = do 
    userFacingError <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler
        }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler = const $ pure ()