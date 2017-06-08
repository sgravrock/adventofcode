module Lib
    ( isNice
    ) where

import Data.List

isNice :: String -> Bool
isNice s = hasDouble s && not (hasBadWords s) && hasVowels 3 s

hasDouble :: String -> Bool
hasDouble (x:xs) = firstIs x xs || hasDouble xs
hasDouble [] = False

firstIs :: Char -> String -> Bool
firstIs c (x:xs) = c == x
firstIs _ [] = False

hasBadWords :: String -> Bool
hasBadWords s = (isInfixOf "xy" s) || (isInfixOf "ab" s) || (isInfixOf "cd" s) || (isInfixOf "pq" s)

hasVowels :: Int -> String -> Bool
hasVowels 0 _ = True
hasVowels n [] = False
hasVowels n (x:xs) 
    | isVowel x = hasVowels (n - 1) xs
    | otherwise = hasVowels n xs

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False
