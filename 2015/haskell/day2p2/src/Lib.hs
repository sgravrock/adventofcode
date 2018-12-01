module Lib
    ( ribbon
    , ribbon1
    , parseLine
    ) where

import Data.List.Split

someFunc = putStrLn "someFunc"

ribbon :: [String] -> Int
ribbon lines =
    let dims = map parseLine lines
        lengths = map ribbon1 dims
    in foldr (+) 0 lengths

ribbon1 :: (Int, Int, Int) -> Int
ribbon1 dims = smallestPerimeter dims + volume dims

smallestPerimeter :: (Int, Int, Int) -> Int
smallestPerimeter dims = foldr min (maxBound :: Int) (sidePerimeters dims)

sidePerimeters :: (Int, Int, Int) -> [Int]
sidePerimeters (x,y,z) = map double [x+y, x+z, y+z]

volume :: (Int, Int, Int) -> Int
volume (x,y,z) = x*y*z

double :: Int -> Int
double x = x * 2

parseLine :: String -> (Int, Int, Int)
parseLine x = 
    let elems = splitOn "x" x
        dims = map read elems
    in (dims!!0, dims!!1, dims!!2)
