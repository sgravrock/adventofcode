module Lib
    ( numHouses
    ) where

import qualified Data.Set as Set

numHouses :: String -> Int
numHouses s = Set.size (visit s (Set.singleton (0,0)) (0,0))

visit :: String -> Set.Set (Int,Int) -> (Int,Int) -> Set.Set (Int,Int)
visit [] visited _ = visited
visit (x:xs) visited pos = 
    let newpos = nextpos x pos
        newvisited = Set.insert newpos visited
    in visit xs newvisited newpos

nextpos :: Char -> (Int,Int) -> (Int,Int)
nextpos '>' (x,y) = (x+1,y)
nextpos '<' (x,y) = (x-1,y)
nextpos '^' (x,y) = (x,y+1)
nextpos 'v' (x,y) = (x,y-1)
nextpos _ _ = error "Bad argument to nextpos"
