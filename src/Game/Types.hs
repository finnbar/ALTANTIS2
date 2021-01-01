{-# LANGUAGE NamedFieldPuns #-}

module Game.Types where

import DiscordUtils.Arguments

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Conc (newTVarIO, TVar, STM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Discord.Types

data CommandEnv = CommandEnv { gs :: GameState, message :: Message }
type Error = Text
type Command a = ExceptT Error (ReaderT CommandEnv STM) a

data GameState = GS { subs :: TVar (Map Text Submarine), worldMap :: TVar WorldMap, users :: TVar (Map User Text) }

makeGameState :: IO GameState
makeGameState = do
    subs <- newTVarIO M.empty
    worldMap <- newTVarIO $ Vec.replicate 10 (Vec.replicate 10 Empty)
    users <- newTVarIO M.empty
    return $ GS {subs, worldMap, users}

type WorldMap = Vector (Vector Square)
data Square = Empty | Barrier

-- TODO: actual full submarine type
data Submarine = Sub { powerSystem :: PowerSystem, steering :: Steering }
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving Show

instance ArgType Direction where
    parse [] = Nothing
    parse ("N":xs) = Just (N, xs)
    parse ("NE":xs) = Just (NE, xs)
    parse ("NW":xs) = Just (NW, xs)
    parse ("S":xs) = Just (S, xs)
    parse ("SE":xs) = Just (SE, xs)
    parse ("SW":xs) = Just (SW, xs)
    parse ("E":xs) = Just (E, xs)
    parse ("W":xs) = Just (W, xs)

data PowerSystem = Power { total :: Int, current :: Int }
data Steering = Steering { direction :: Direction, position :: (Int, Int)}