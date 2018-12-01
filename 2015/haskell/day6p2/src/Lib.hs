module Lib
    ( Point(..)
    , XRange(..)
    , YRange(..)
    , CmdType(..)
    , Cmd(..)
    , runScript
    , parseCmd
    , execute
    , sumValues
    ) where

import qualified Data.Map.Strict as Map
import Data.List.Split

data Point = Point Int Int deriving(Show, Ord, Eq)
data XRange = XRange Int Int deriving(Show, Eq)
data YRange = YRange Int Int deriving(Show, Eq)
data CmdType = On | Off | Toggle deriving(Show, Eq)
data Cmd = Cmd CmdType XRange YRange deriving(Show, Eq)

type Grid = Map.Map Point Int

runScript :: [String] -> Int
runScript strings =
    let commands = map parseCmd strings
        input = Map.empty
        output = foldl execute input commands
    in sumValues output

parseCmd :: String -> Cmd
parseCmd s
    | words!!0 == "toggle" = parseToggle words
    | otherwise = parseOnOff words
    where words = splitOn " " s

parseToggle :: [String] -> Cmd
parseToggle words = 
    let (xr, yr) = parseRanges (words!!1) (words!!3)
    in Cmd Toggle xr yr

parseOnOff :: [String] -> Cmd
parseOnOff words
    | cmdWord == "on" = Cmd On xr yr
    | cmdWord == "off" = Cmd Off xr yr
    | otherwise = error ("Unrecognized command: " ++ cmdWord)
    where
        cmdWord = words!!1
        (xr, yr) = parseRanges (words!!2) (words!!4)

parseRanges :: String -> String -> (XRange, YRange)
parseRanges startWord endWord =
    let (x0, y0) = parseNumPair startWord
        (x1, y1) = parseNumPair endWord
    in ((XRange x0 x1), (YRange y0 y1))

parseNumPair :: String -> (Int, Int)
parseNumPair s =
    let tokens = splitOn "," s
        a = read (tokens!!0)
        b = read (tokens!!1)
    in (a, b)

execute :: Grid -> Cmd -> Grid
execute s (Cmd cmdType xr yr) = mapRegion s xr yr (cmdFn cmdType)

cmdFn :: CmdType -> (Int -> Int)
cmdFn On = \x -> x + 1
cmdFn Off = \x -> max 0 (x - 1)
cmdFn Toggle = \x -> x + 2

mapRegion :: Grid -> XRange -> YRange -> (Int -> Int) -> Grid
mapRegion input (XRange x0 x1) yrange f
    | x0 == x1 = modified
    | otherwise = mapRegion modified (XRange (x0+1) x1) yrange f
    where modified = mapCol input x0 yrange f

mapCol :: Grid -> Int -> YRange -> (Int -> Int) -> Grid
mapCol input x (YRange y0 y1) f
    | y0 == y1 = modified
    | otherwise = mapCol modified x (YRange (y0+1) y1) f
    where modified = mapPoint input (Point x y0) f

mapPoint :: Grid -> Point -> (Int -> Int) -> Grid
mapPoint input p f = Map.insert p (f oldval) input
    where oldval = valueAt input p

valueAt :: Grid -> Point -> Int
valueAt input p = case Map.lookup p input of Just n -> n
                                             Nothing -> 0

sumValues :: Map.Map k Int -> Int
sumValues m = Map.foldr (+) 0 m
