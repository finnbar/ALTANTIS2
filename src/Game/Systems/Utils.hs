module Game.Systems.Utils where

import Game.Types

import Discord.Types
import Data.Text (Text)
import GHC.Conc
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except

toEither :: Monad m => b -> Maybe a -> ExceptT b m a
toEither err Nothing  = throwE err
toEither _   (Just x) = return x

-- TODO: move command framework from Types.hs to its own file, also split up Types.hs in a nicer way.
-- in doing so, probably nick MonadSTM from concurrency

getTeam :: Command Text
getTeam = do
    gamestate <- asks gs
    u <- asks $ messageAuthor . message
    usermap <- lift . lift $ readTVar (users gamestate)
    toEither "User isn't attributed to a team!" $ Map.lookup u usermap

getSub :: Text -> Command Submarine
getSub t = do
    gamestate <- asks gs
    submarines <- lift . lift $ readTVar (subs gamestate)
    toEither "That team doesn't have a submarine!" $ Map.lookup t submarines

getYourSub :: Command Submarine
getYourSub = getTeam >>= getSub