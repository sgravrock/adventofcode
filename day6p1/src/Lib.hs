module Lib
    ( isNice
    , Point(..)
    , XRange(..)
    , YRange(..)
    , Cmd(..)
    , execute
    ) where

import qualified Data.Set as Set

isNice :: String -> Bool
isNice _ = False

data Point = Point Int Int deriving(Show, Ord, Eq)
data XRange = XRange Int Int deriving(Show)
data YRange = YRange Int Int deriving(Show)
data Cmd = On | Off | Toggle

type Grid = Set.Set Point

execute :: Grid -> Cmd -> XRange -> YRange -> Grid
execute s cmd xr yr = update s xr yr (cmdFn cmd)

cmdFn :: Cmd -> (Bool -> Bool)
cmdFn On = \_ -> True
cmdFn Off = \_ -> False
cmdFn Toggle = \x -> not x

update :: Grid -> XRange -> YRange -> (Bool -> Bool) -> Grid
update input (XRange x0 x1) yrange f
    | x0 == x1 = modified
    | otherwise = update modified (XRange (x0+1) x1) yrange f
    where modified = updateCol input x0 yrange f

updateCol :: Grid -> Int -> YRange -> (Bool -> Bool) -> Grid
updateCol input x (YRange y0 y1) f
    | y0 == y1 = modified
    | otherwise = updateCol modified x (YRange (y0+1) y1) f
    where modified = updatePoint input (Point x y0) f

updatePoint :: Grid -> Point -> (Bool -> Bool) -> Grid
updatePoint input p f
    | newval = Set.insert p input
    | otherwise = Set.delete p input
    where
        oldval = Set.member p input
        newval = f oldval
