module Lib
    ( wrappingPaper
    , wrappingPaper1
    , smallestArea
    , sideAreas
    , parseLine
    ) where

import Data.List.Split

someFunc = putStrLn "someFunc"

wrappingPaper :: [String] -> Int
wrappingPaper lines = 
    let dims = map parseLine lines
        areas = map wrappingPaper1 dims
    in foldr (+) 0 areas

wrappingPaper1 :: (Int, Int, Int) -> Int
wrappingPaper1 dims = 
    let coverage = foldr (+) 0 (map double (sideAreas dims))
        extra = smallestArea dims
    in coverage + extra

smallestArea :: (Int, Int, Int) -> Int
smallestArea dims = foldr min (maxBound :: Int) (sideAreas dims)

sideAreas :: (Int, Int, Int) -> [Int]
sideAreas (x,y,z) = [x*y, x*z, y*z]

double :: Int -> Int
double x = x * 2

parseLine :: String -> (Int, Int, Int)
parseLine x = 
    let elems = splitOn "x" x
        dims = map read elems
    in (dims!!0, dims!!1, dims!!2)
