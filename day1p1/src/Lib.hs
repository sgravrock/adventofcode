module Lib
    ( someFunc
    , floorReached
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

floorReached :: String -> Integer
floorReached s = foldr (+) 0 (map direction s)

direction :: Char -> Integer
direction '(' = 1
direction ')' = -1
direction x = 0
