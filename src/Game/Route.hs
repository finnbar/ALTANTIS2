module Game.Route
    ( doCommand
    , GameState) where

import DiscordUtils.Utils
import DiscordUtils.Arguments
import Game.Types
import Game.Commands
import Game.Systems.Movement

import Discord
import Discord.Types

doCommand :: Message -> GameState -> DiscordHandler ()
doCommand m gs = runCommand m gs $
    let (comm, args) = getCommandAndArgs $ messageText m
    in case comm of
        "setdir" -> setDirection (parseArgs args :: Maybe Direction)