module DiscordUtils.Arguments where

import DiscordUtils.Utils (tread)

import Data.Text (Text)

parseArgs :: ArgType t => [Text] -> Maybe t
parseArgs ts = parse ts >>=
    \(v, ts) -> case ts of
                  [] -> Just v
                  _  -> Nothing

-- Essentially our parser state.
type AT t = Maybe (t, [Text])

class ArgType t where
    parse :: [Text] -> AT t

-- Single values.
instance ArgType Text where
    parse [] = Nothing
    parse (x:xs) = Just (x, xs)

instance ArgType Int where
    parse [] = Nothing
    parse (x:xs) = (tread x :: Maybe Int) >>= \v -> Just (v, xs)

instance ArgType Bool where
    parse [] = Nothing
    parse ("True":xs)  = Just (True, xs)
    parse ("true":xs)  = Just (True, xs)
    parse ("False":xs) = Just (False, xs)
    parse ("false":xs) = Just (False, xs)
    parse _            = Nothing

-- Optional single values.
instance (ArgType a) => ArgType (Maybe a) where
    parse xs = case parse xs :: AT a of
                 Nothing       -> Just (Nothing, xs)
                 Just (v, xs') -> Just (Just v, xs')

-- Lists of values (minimum zero).
instance (ArgType a) => ArgType [a] where
    parse [] = Just ([], [])
    parse xs = case (parse xs :: AT a) of
                 Nothing       -> Just ([], xs)
                 Just (v, xs') -> (parse xs' :: AT [a]) >>=
                     \(v', xs'') -> Just (v:v', xs'')

-- Tuple types
instance (ArgType a, ArgType b) => ArgType (a, b) where
    parse [] = Nothing
    parse xs = do
        (v, xs') <- (parse xs :: AT a)
        (v', xs'') <- (parse xs' :: AT b)
        return ((v, v'), xs'')
