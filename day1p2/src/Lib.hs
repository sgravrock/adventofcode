module Lib
    ( someFunc
    , stepsToBasement
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stepsToBasement :: String -> Integer
stepsToBasement s = steps' 0 s

steps' :: Integer -> String -> Integer
steps' (-1) s = 0
steps' n (x:xs) = 1 + steps' (n + direction x) xs

floorReached :: String -> Integer
floorReached s = foldr (+) 0 (map direction s)

direction :: Char -> Integer
direction '(' = 1
direction ')' = -1
direction x = 0
