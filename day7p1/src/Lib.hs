module Lib
    ( signalOnWire
    , parseWire
    , WireSpec(..)
    , Equation(..)
    ) where

import Data.List.Split

data WireSpec = WireSpec String Equation deriving (Show, Eq)
data Equation = Const Int
              | And String String
              | Or String String
              | LShift String Int
              | RShift String Int
              | Not String
              deriving (Show, Eq)

signalOnWire :: String -> [String] -> Maybe Int
signalOnWire _ _ = Nothing

parseWire :: String -> WireSpec
parseWire line
    | length tokens == 3 = parseConst tokens
    | length tokens == 4 = parseNot tokens
    | length tokens == 5 = parseBinary tokens
    | otherwise = error ("Can't parse this wire: " ++ line)
    where tokens = splitOn " " line :: [String]

parseConst :: [String] -> WireSpec
parseConst tokens = WireSpec (tokens!!2) (Const (read (tokens!!0)))

parseNot :: [String] -> WireSpec
parseNot tokens = WireSpec (tokens!!3) (Not (tokens!!1))

parseBinary :: [String] -> WireSpec
parseBinary tokens
    | operator == "AND" = WireSpec dest (And (tokens!!0) (tokens!!2)) 
    | operator == "OR" = WireSpec dest (Or (tokens!!0) (tokens!!2)) 
    | operator == "LSHIFT" = WireSpec dest (LShift (tokens!!0) (read (tokens!!2)))
    | operator == "RSHIFT" = WireSpec dest (RShift (tokens!!0) (read (tokens!!2)))
    | otherwise = error ("Can't parse this wire: " ++ (show tokens))
    where
        operator = tokens!!1
        dest = tokens!!4
