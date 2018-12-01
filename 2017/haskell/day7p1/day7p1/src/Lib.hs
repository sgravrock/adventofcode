module Lib
    ( NodeSpec(..)
    , parseInput
    , root
    ) where

import Data.List.Split


data NodeSpec = NodeSpec { name :: String
                         , childNames :: [String]
                         } deriving (Eq, Show)

parseInput :: [String] -> [NodeSpec]
parseInput input = map parseLine input

parseLine :: String -> NodeSpec
parseLine line =
    let words = splitOn " " (filter (\c -> c /= ',') line)
    in NodeSpec { name=words!!0, childNames=drop 3 words }

root :: [NodeSpec] -> String
root specs = 
    let unmentioned = filter (withoutParent specs) specs
    in name (unmentioned!!0)

withoutParent :: [NodeSpec] -> NodeSpec -> Bool
withoutParent specs specToFind =
    let nameToFind = name specToFind
    in not (contains (hasChild nameToFind) specs)

hasChild :: String -> NodeSpec -> Bool
hasChild name spec = elem name (childNames spec)

contains :: (a -> Bool) -> [a] -> Bool
contains predicate xs =
    let matches = filter predicate xs
    in length matches /= 0
