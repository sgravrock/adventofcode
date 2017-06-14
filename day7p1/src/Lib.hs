module Lib
    ( signalOnWire
    , parseWire
    , WireSpec(..)
    ) where

import Data.List.Split

data WireSpec = ConstSpec String Int
              | AndSpec String String String
              | OrSpec String String String
              | LShiftSpec String String Int
              | RShiftSpec String String Int
              | NotSpec String String
              deriving (Show, Eq)

signalOnWire :: String -> [String] -> Maybe Int
signalOnWire _ _ = Nothing

parseWire :: String -> WireSpec
parseWire line
    | length tokens == 3 = ConstSpec (tokens!!2) (read (tokens!!0))
    | length tokens == 4 = NotSpec (tokens!!3) (tokens!!1)
    | length tokens == 5 && tokens!!1 == "AND" =
        AndSpec (tokens!!4) (tokens!!0) (tokens!!2)
    | length tokens == 5 && tokens!!1 == "OR" =
        OrSpec (tokens!!4) (tokens!!0) (tokens!!2)
    | length tokens == 5 && tokens!!1 == "LSHIFT" =
        LShiftSpec (tokens!!4) (tokens!!0) (read (tokens!!2))
    | length tokens == 5 && tokens!!1 == "RSHIFT" =
        RShiftSpec (tokens!!4) (tokens!!0) (read (tokens!!2))
    | otherwise = error ("Can't parse this wire: " ++ line)
    where tokens = splitOn " " line :: [String]
