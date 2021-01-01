module Game.Commands
    ( Command
    , runCommand
    ) where

import Game.Types
import DiscordUtils.Utils

import Discord (DiscordHandler)
import Discord.Types (Message(..), User)
import Control.Monad.IO.Class
import Data.Text (Text)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Concurrent.STM (atomically)

runCommand :: Message -> GameState -> Command Text -> DiscordHandler ()
runCommand m gs comm = do
    let env = CommandEnv gs m
    let stm = runReaderT (runExceptT comm) env
    res <- liftIO $ atomically stm
    case res of
        Left err -> do
            _ <- reactToCommand m ":negative_squared_cross_mark:"
            sendMessage m err
        Right out -> do
            _ <- reactToCommand m ":white_check_mark:"
            sendMessage m out
    pure ()