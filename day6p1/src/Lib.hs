module Lib
    ( isNice
    , Point(..)
    , Range(..)
    , Cmd(..)
    , execute
    ) where

import qualified Data.Set as Set

isNice :: String -> Bool
isNice _ = False

data Point = Point Int Int deriving(Show)
data Range = Range Int Int deriving(Show)
data Cmd = On Range Range | Off Range Range | Toggle Range Range

type Grid = Set.Set Point

execute :: Grid -> Cmd -> Grid
execute s (On xr yr) = s
execute s _ = s

--update :: Grid -> Range -> Range -> (Bool -> Bool) -> Grid
--update input xr yr f = f
--
updatePoint :: Grid -> Point -> (Bool -> Bool) -> Grid
updatePoint input p f =
    let oldval = Set.member p input
        newval = f oldval
    in Set.insert p 
