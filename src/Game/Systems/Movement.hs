module Game.Systems.Movement
    ( setDirection
    ) where

import Game.Types
import Game.Systems.Utils

import Data.Text (Text)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- TODO: work out if there's a better way to structure Command.
-- We want something that allows us to first combine arguments that might not exist.
-- Then we want a Reader to collect arguments from the environment, and finally we build an STM that might fail.
-- Question: is it going to fail if we can guarantee that all arguments are there?
-- Either way, we can drop the requirement for the internal Either and have it for things that could fail.

setDirection :: Maybe Direction -> Command
setDirection d = case d of
    Nothing  -> pure $ Left "You must provide a valid compass direction!"
    Just dir -> do
        t <- getTeam
        case t of
            Left err -> pure $ Left err
            Right tm -> do
                gamestate <- asks $ subs . gs
                lift $ modifyTVar' gamestate (Map.adjust setDir tm)
                return $ Right ""
                    where setDir sub = sub { steering = (steering sub) {direction = dir} }