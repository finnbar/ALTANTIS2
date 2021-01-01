module Game.Systems.Utils where

import Game.Types

import Discord.Types
import Data.Text (Text)
import GHC.Conc
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

getTeam :: ReaderT CommandEnv STM (Either Error Text)
getTeam = do
    gamestate <- asks gs
    u <- asks $ messageAuthor . message
    lift $ do
        usermap <- readTVar (users gamestate)
        case Map.lookup u usermap of
            Nothing -> return $ Left "User isn't attributed to a team!"
            Just t -> return $ Right t

getSub :: Text -> ReaderT CommandEnv STM (Either Error Submarine)
getSub t = do
    gamestate <- asks gs
    lift $ do
        submarines <- readTVar (subs gamestate)
        case Map.lookup t submarines of
            Nothing -> return $ Left "That team doesn't have a submarine!"
            Just sub -> return $ Right sub

getYourSub :: ReaderT CommandEnv STM (Either Error Submarine)
getYourSub = do
    t <- getTeam
    case t of
        Left err -> return $ Left err
        Right tm -> getSub tm