module Lib
    ( Node(..)
    , parseInput
    , root
    , linkNodes
    ) where

import Data.List
import Data.List.Split


data Node = Node { name :: String
                 , childNames :: [String]
                 , children :: [Node]
                 } deriving (Eq, Show)


parseInput :: [String] -> [Node]
parseInput input = map parseLine input

parseLine :: String -> Node
parseLine line =
    let words = splitOn " " (filter (\c -> c /= ',') line)
    in Node { name=words!!0, childNames=drop 3 words, children=[] }

root :: [Node] -> String
root specs = name (makeTree specs)

makeTree :: [Node] -> Node
makeTree nodes = findLinkedRoot (linkNodes nodes)

linkNodes :: [Node] -> [Node]
linkNodes nodes = foldr linkOne nodes nodes

linkOne :: Node -> [Node] -> [Node]
linkOne nodeToLink nodes =
    case nodes of
        [] -> error ("No parent for node " ++ (name nodeToLink))
        [nodeToLink] -> [nodeToLink]
        (x:xs) ->
            case maybeAddChild x nodeToLink of
                Just newX -> newX:xs
                Nothing -> x:(linkOne nodeToLink xs)

maybeAddChild :: Node -> Node -> Maybe Node
maybeAddChild parent child
    | hasChild parent child = Just parent {children = child:(children parent)}
    | otherwise = Nothing

hasChild :: Node -> Node -> Bool
hasChild parent child = elem (name child) (childNames parent)

findLinkedRoot :: [Node] -> Node
findLinkedRoot nodes =
    case filter (\x -> withoutParent x nodes) nodes of
        [one] -> one
        otherwise -> error "Found multiple roots"

withoutParent :: Node -> [Node] -> Bool
withoutParent node nodes =
    case parent node nodes of
        Nothing -> True
        Just _ -> False

parent :: Node -> [Node] -> Maybe Node
parent child candidates =
    case filter (\mp -> hasChild mp child) candidates of
        [p] -> Just p
        [] -> Nothing
        otherwise -> error ("Found multiple parents of " ++ (name child))
