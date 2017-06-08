module Lib
    ( numHouses
    ) where

import qualified Data.Set as Set

type Coord = (Int, Int)

numHouses :: String -> Int
numHouses s = 
    let start = (0, 0)
    in Set.size (visit s (Set.singleton start) start start)

visit :: String -> Set.Set Coord -> Coord -> Coord -> Set.Set Coord
visit [] visited _ _ = visited
visit (x:xs) visited santa1pos santa2pos = 
    let newpos1 = nextpos x santa1pos
        newvisited = Set.insert newpos1 visited
    in visit xs newvisited santa2pos newpos1

nextpos :: Char -> Coord -> Coord
nextpos '>' (x,y) = (x+1,y)
nextpos '<' (x,y) = (x-1,y)
nextpos '^' (x,y) = (x,y+1)
nextpos 'v' (x,y) = (x,y-1)
nextpos _ _ = error "Bad argument to nextpos"
