module Lib
    ( signalOnWire
    , parseWire
    , WireSpec(..)
    , Equation(..)
    , Rvalue(..)
    ) where

import Data.Word
import Data.Bits
import Data.List.Split
import Text.Read

data WireSpec = WireSpec String Equation deriving (Show, Eq)
data Equation = Assign Rvalue
              | And Rvalue Rvalue
              | Or Rvalue Rvalue
              | LShift String Word16
              | RShift String Word16
              | Not String
              deriving (Show, Eq)
data Rvalue = Ref String
            | Const Word16
            deriving (Show, Eq)

signalOnWire :: String -> [WireSpec] -> Word16
signalOnWire name specs = signalOnWire' (findWire name specs) specs

signalOnWire' :: Equation -> [WireSpec] -> Word16
signalOnWire' (Assign rvalue) circuit = expand rvalue circuit
signalOnWire' (Not operand) circuit =
    complement (signalOnWire operand circuit)
signalOnWire' (And x y) circuit = (expand x circuit) .&. (expand y circuit)
signalOnWire' (Or x y) circuit = (expand x circuit) .|. (expand y circuit)
signalOnWire' (LShift x y) circuit = shiftWire x y 1 circuit
signalOnWire' (RShift x y) circuit = shiftWire x y (-1) circuit

shiftWire :: String -> Word16 -> Int -> [WireSpec] -> Word16
shiftWire name offset multiplier circuit =
    let lhs = signalOnWire name circuit
        rhs = multiplier * (fromIntegral offset)
    in shift lhs rhs

expand :: Rvalue -> [WireSpec] -> Word16
expand (Const x) _ = x
expand (Ref wn) circuit = signalOnWire wn circuit

findWire :: String -> [WireSpec] -> Equation
findWire targetName ((WireSpec name eqn):xs)
    | name == targetName = eqn
    | otherwise = findWire targetName xs
findWire targetName [] = error ("Can't find wire " ++ targetName)

parseWire :: String -> WireSpec
parseWire line
    | length tokens == 3 = parseAssign tokens
    | length tokens == 4 = parseNot tokens
    | length tokens == 5 = parseBinary tokens
    | otherwise = error ("Can't parse this wire: " ++ line)
    where tokens = splitOn " " line :: [String]

parseAssign :: [String] -> WireSpec
parseAssign tokens = WireSpec (tokens!!2) (Assign (parseRvalue (tokens!!0)))

parseNot :: [String] -> WireSpec
parseNot tokens = WireSpec (tokens!!3) (Not (tokens!!1))

parseBinary :: [String] -> WireSpec
parseBinary tokens
    | operator == "AND" = WireSpec dest
        (And (parseRvalue (tokens!!0)) (parseRvalue (tokens!!2)))
    | operator == "OR" = WireSpec dest
        (Or (parseRvalue (tokens!!0)) (parseRvalue (tokens!!2)))
    | operator == "LSHIFT" = WireSpec dest (LShift (tokens!!0) (readWord16 tokens 2))
    | operator == "RSHIFT" = WireSpec dest (RShift (tokens!!0) (readWord16 tokens 2))
    | otherwise = error ("Can't parse this wire: " ++ (show tokens))
    where
        operator = tokens!!1
        dest = tokens!!4

parseRvalue :: String -> Rvalue
parseRvalue s = case readMaybe s of
                    (Just n) -> Const n
                    Nothing -> Ref s

readWord16 :: [String] -> Int -> Word16
readWord16 words i = case readMaybe (words!!i) of
                        (Just n) -> n
                        Nothing -> error ("Expected a number but got " ++ (words!!i) ++ "(in " ++ show words ++ ")")
