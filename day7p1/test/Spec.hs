import Control.Monad
import System.Exit
import Test.HUnit

import Lib

sampleCircuit = map parseWire [ "123 -> x"
                              , "456 -> y"
                              , "x AND y -> d"
                              , "x OR y -> e"
                              , "x LSHIFT 2 -> f"
                              , "y RSHIFT 2 -> g"
                              , "NOT x -> h"
                              , "NOT y -> i"
                              ]

tests = test [ "sample d" ~: 72 ~=? signalOnWire "d" sampleCircuit
             , "sample e" ~: 507 ~=? signalOnWire "e" sampleCircuit
             , "sample f" ~: 492 ~=? signalOnWire "f" sampleCircuit
             , "sample g" ~: 114 ~=? signalOnWire "g" sampleCircuit
             , "sample h" ~: 65412 ~=? signalOnWire "h" sampleCircuit
             , "sample i" ~: 65079 ~=? signalOnWire "i" sampleCircuit
             , "sample x" ~: 123 ~=? signalOnWire "x" sampleCircuit
             , "sample y" ~: 456 ~=? signalOnWire "y" sampleCircuit
             , "eval assign ref" ~: 42 ~=? signalOnWire "b"
                [(parseWire "42 -> a"), (parseWire  "a -> b")]
             , "eval and const" ~: 72 ~=?
                signalOnWire "a" [parseWire "123 AND 456 -> a"]
             , "eval or const" ~: 507 ~=?
                signalOnWire "a" [parseWire "123 OR 456 -> a"]
             , "parse assign const" ~: WireSpec "x" (Assign (Const 123)) ~=?
                parseWire "123 -> x"
             , "parse assign ref" ~: WireSpec "x" (Assign (Ref "y")) ~=?
                parseWire "y -> x"
             , "parse and" ~:
                WireSpec "d" (And (Ref "x") (Ref "y")) ~=?
                parseWire "x AND y -> d"
             , "parse or" ~: WireSpec "d" (Or (Ref "x") (Ref "y")) ~=?
                parseWire "x OR y -> d"
             , "parse lshift" ~: WireSpec "f" (LShift "x" 2) ~=?
                parseWire "x LSHIFT 2 -> f"
             , "parse rshift" ~: WireSpec "f" (RShift "x" 2) ~=?
                parseWire "x RSHIFT 2 -> f"
             , "parse not" ~: WireSpec "h" (Not "x") ~=?
                parseWire "NOT x -> h"
             , "parse and const" ~:
                WireSpec "d" (And (Const 4) (Const 5)) ~=?
                parseWire "4 AND 5 -> d"
             , "parse or const" ~: 
                WireSpec "d" (Or (Const 4) (Const 5)) ~=?
                parseWire "4 OR 5 -> d"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
