module DiscordUtils.Utils where

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Prelude hiding (words, unwords)
import Data.Text (Text, words, unwords, stripPrefix, stripSuffix, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Text.Read (readMaybe)

type DiscordRest a = DiscordHandler (Either RestCallErrorCode a)

sendMessage :: Message -> Text -> DiscordRest Message
sendMessage m t = restCall $ R.CreateMessage (messageChannel m) t

reactToCommand :: Message -> Text -> DiscordRest ()
reactToCommand m e = restCall $ R.CreateReaction (messageChannel m, messageId m) e

getCommandAndArgs :: Text -> (Text, [Text])
getCommandAndArgs mtext = (command, rest)
    where wordList = words mtext
          command  = head wordList
          rest     = quoteDelimited [] $ tail wordList

-- Joins together regions with StartQuoted (NotQuoted*) EndQuoted
data Quoted = StartQuoted Text | EndQuoted Text | NotQuoted Text
isQuote :: Text -> Quoted
isQuote t = case stripPrefix "\"" t of
              Just suff -> case stripSuffix "\"" suff of
                             Just pref -> NotQuoted pref
                             Nothing   -> StartQuoted suff
              Nothing   -> case stripSuffix "\"" t of
                             Just pref -> EndQuoted pref
                             Nothing   -> NotQuoted t

quoteDelimited :: [Text] -> [Text] -> [Text]
quoteDelimited buff [] = reverse buff
quoteDelimited buff (x:xs) =
    case isQuote x of
      NotQuoted s   -> quoteDelimited (s:buff) xs
      EndQuoted s   -> quoteDelimited (s:buff) xs
      StartQuoted s -> case quoteDelimited' xs of
                         Just (s', xs') -> quoteDelimited ((unwords $ s:s'):buff) xs'
                         Nothing -> quoteDelimited (s:buff) xs

    -- Repeat the search until you get an EndQuoted
        where quoteDelimited' [] = Nothing
              quoteDelimited' (x:xs) =
                  case isQuote x of
                    EndQuoted s -> Just ([s], xs)
                    NotQuoted s -> quoteDelimited' xs >>= \(s', xs') -> Just (s:s', xs')
                    StartQuoted s -> quoteDelimited' xs >>= \(s', xs') -> Just (s:s', xs')

tshow :: (Show a) => a -> Text
tshow = pack . show

tread :: (Read a) => Text -> Maybe a
tread = readMaybe . unpack
