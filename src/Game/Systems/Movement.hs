module Game.Systems.Movement
    ( setDirection
    ) where

import Game.Types
import Game.Systems.Utils

import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Reader

setDirection :: Maybe Direction -> Command ()
setDirection d = do
    dir <- toEither "You must provide a valid compass direction!" d
    t <- getTeam
    gamestate <- asks $ subs . gs
    let setDir sub = sub { steering = (steering sub) {direction = dir} }
    lift . lift $ modifyTVar' gamestate (Map.adjust setDir t)