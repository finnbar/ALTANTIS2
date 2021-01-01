module Game.Commands
    ( Command
    , runCommand
    ) where

import Game.Types
import DiscordUtils.Utils

import Discord (DiscordHandler)
import Discord.Types (Message(..), User)
import GHC.Conc
import Control.Monad.IO.Class
import Data.Text (Text)
import Control.Monad.Trans.Reader

runCommand :: Message -> GameState -> Command -> DiscordHandler ()
runCommand m gs comm = do
    let env = CommandEnv gs m
    let stm = runReaderT comm env
    res <- liftIO $ atomically stm
    case res of
        Left err -> do
            _ <- reactToCommand m ":negative_squared_cross_mark:"
            sendMessage m err
        Right out -> do
            _ <- reactToCommand m ":white_check_mark:"
            sendMessage m out
    pure ()

-- TODO: actually make this find the relevant submariner
getTeam :: User -> Maybe Text
getTeam = const Nothing