module Lib
    ( hash
    , goodHash
    , nextCoin
    , hasPrefix
    ) where

import Data.Hash.MD5

hash :: String -> String
hash = md5s . Str

nextCoin :: String -> Int -> Int
nextCoin key n
    | goodHash h = n
    | n > 609043 = error "nope"
    | otherwise = nextCoin key n + 1
    where h = hash (key ++ show n)

goodHash :: String -> Bool
goodHash s = hasPrefix s "00000"

hasPrefix :: String -> String -> Bool
hasPrefix _ [] = True
hasPrefix (x:xs) (p:ps) = x == p && hasPrefix xs ps
