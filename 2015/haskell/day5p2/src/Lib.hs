module Lib
    ( isNice
    , hasRepeatingPair
    , hasTriplet
    ) where

import Data.List

isNice :: String -> Bool
isNice s = hasRepeatingPair s && hasTriplet s

hasRepeatingPair :: String -> Bool
hasRepeatingPair [] = False
hasRepeatingPair [x] = False
hasRepeatingPair (x:(y:xs)) = isInfixOf [x,y] xs || hasRepeatingPair (y:xs)

hasTriplet :: String -> Bool
hasTriplet (x:(y:(z:xs))) = x == z || hasTriplet (y:(z:xs))
hasTriplet _ = False
