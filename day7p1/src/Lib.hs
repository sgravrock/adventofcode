module Lib
    ( signalOnWire
    , parseWire
    , WireSpec(..)
    , Equation(..)
    , Rvalue(..)
    ) where

import Data.List.Split
import Text.Read

data WireSpec = WireSpec String Equation deriving (Show, Eq)
data Equation = Assign Int
              | And Rvalue Rvalue
              | Or Rvalue Rvalue
              | LShift String Int
              | RShift String Int
              | Not String
              deriving (Show, Eq)
data Rvalue = Ref String
            | Const Int
            deriving (Show, Eq)

signalOnWire :: String -> [String] -> Maybe Int
signalOnWire _ _ = Nothing

parseWire :: String -> WireSpec
parseWire line
    | length tokens == 3 = parseAssign tokens
    | length tokens == 4 = parseNot tokens
    | length tokens == 5 = parseBinary tokens
    | otherwise = error ("Can't parse this wire: " ++ line)
    where tokens = splitOn " " line :: [String]

parseAssign :: [String] -> WireSpec
parseAssign tokens = WireSpec (tokens!!2) (Assign (read (tokens!!0)))

parseNot :: [String] -> WireSpec
parseNot tokens = WireSpec (tokens!!3) (Not (tokens!!1))

parseBinary :: [String] -> WireSpec
parseBinary tokens
    | operator == "AND" = WireSpec dest
        (And (parseRvalue (tokens!!0)) (parseRvalue (tokens!!2)))
    | operator == "OR" = WireSpec dest
        (Or (parseRvalue (tokens!!0)) (parseRvalue (tokens!!2)))
    | operator == "LSHIFT" = WireSpec dest (LShift (tokens!!0) (read (tokens!!2)))
    | operator == "RSHIFT" = WireSpec dest (RShift (tokens!!0) (read (tokens!!2)))
    | otherwise = error ("Can't parse this wire: " ++ (show tokens))
    where
        operator = tokens!!1
        dest = tokens!!4

parseRvalue :: String -> Rvalue
parseRvalue s = case readMaybe s of
                    (Just n) -> Const n
                    Nothing -> Ref s
