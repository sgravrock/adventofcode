module Lib
    ( unescape
    , delta
    ) where

import Text.Read

delta :: [String] -> Int
delta strings = let escLen = foldr (+) 0 (map length strings)
                    unescLen = foldr (+) 0 (map (length . unescape) strings)
                in escLen - unescLen

unescape :: String -> String
unescape = unescapeBs . unescapeHex . stripLeadingQuote . stripTrailingQuote

stripLeadingQuote :: String -> String
stripLeadingQuote s = case s of ('"':xs) -> xs
                                otherwise -> s

stripTrailingQuote :: String -> String
stripTrailingQuote s
    | length s == 0 = ""
    | last s == '"' = init s
    | otherwise = s


unescapeHex :: String -> String
unescapeHex s = case s of ('\\':('x':(a:(b:xs)))) -> unescapeHex' a b xs
                          (x:xs) -> (x:(unescapeHex xs))
                          otherwise -> s

unescapeHex' :: Char -> Char -> String -> String
unescapeHex' a b xs = case readMaybe (['0', 'x', a, b]) of 
    Just n -> ((toEnum n):xs)
    Nothing -> (a:(b:xs))

unescapeBs :: String -> String
unescapeBs s = case s of ('\\':(c:xs)) -> (c:(unescapeBs xs))
                         (x:xs) -> (x:(unescapeBs xs))
                         otherwise -> s
