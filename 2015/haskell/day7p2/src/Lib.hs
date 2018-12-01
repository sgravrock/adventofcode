module Lib
    ( signalOnWire
    , parseWire
    , WireSpec(..)
    , Equation(..)
    , Rvalue(..)
    ) where

import Data.Word
import Data.Bits
import qualified Data.Map as Map
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

type Memos = Map.Map String Word16

signalOnWire :: String -> [WireSpec] -> Word16
signalOnWire name specs = 
    let (result, _) = memoizedSignalOnWire name specs Map.empty
    in result

memoizedSignalOnWire :: String -> [WireSpec] -> Memos -> (Word16, Memos)
memoizedSignalOnWire name specs memos0 = case Map.lookup name memos0 of
    Just n -> (n, memos0)
    Nothing -> let (result, memos1) = signalOnWire' (findWire name specs) specs memos0
        in (result, Map.insert name result memos1)

signalOnWire' :: Equation -> [WireSpec] -> Memos -> (Word16, Memos)
signalOnWire' (Assign rvalue) circuit memos = expand rvalue circuit memos
signalOnWire' (Not operand) circuit memos =
    let (opVal, newMemos) = memoizedSignalOnWire operand circuit memos
    in (complement opVal, newMemos)
signalOnWire' (And lhs rhs) circuit memos0 =
    let (x, memos1) = (expand lhs circuit memos0)
        (y, memos2) = (expand rhs circuit memos1)
    in (x .&. y, memos2)
signalOnWire' (Or lhs rhs) circuit memos0 =
    let (x, memos1) = (expand lhs circuit memos0)
        (y, memos2) = (expand rhs circuit memos1)
    in (x .|. y, memos2)
signalOnWire' (LShift x y) circuit memos = shiftWire x y 1 circuit memos
signalOnWire' (RShift x y) circuit memos = shiftWire x y (-1) circuit memos

shiftWire :: String -> Word16 -> Int -> [WireSpec] -> Memos -> (Word16, Memos)
shiftWire name offset multiplier circuit memos =
    let (lhs, newMemos) = memoizedSignalOnWire name circuit memos
        rhs = multiplier * (fromIntegral offset)
    in (shift lhs rhs, newMemos)

expand :: Rvalue -> [WireSpec] -> Memos -> (Word16, Memos)
expand (Const x) _ memos = (x, memos)
expand (Ref wn) circuit memos = memoizedSignalOnWire wn circuit memos

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
