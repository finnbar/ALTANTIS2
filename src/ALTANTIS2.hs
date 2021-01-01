module ALTANTIS2
    ( doDiscord
    , makeGameState 
    ) where

import Game.Route (doCommand)
import Game.Types (GameState, makeGameState)

import Discord
import Discord.Types

import Data.Text
import Control.Monad (unless)

import qualified Data.Text.IO as TIO (putStrLn)

doDiscord :: Text -> GameState -> IO ()
doDiscord token gs = do 
    gs <- makeGameState
    userFacingError <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler gs
        }
    TIO.putStrLn userFacingError

eventHandler :: GameState -> Event -> DiscordHandler ()
eventHandler gs ev = case ev of
    MessageCreate m -> unless (fromBot m) $ doCommand m gs

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)