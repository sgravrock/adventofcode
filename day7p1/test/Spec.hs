import Control.Monad
import System.Exit
import Test.HUnit

import Lib

sampleCircuit = [ "123 -> x"
                , "456 -> y"
                , "x AND y -> d"
                , "x OR y -> e"
                , "x LSHIFT 2 -> f"
                , "y RSHIFT 2 -> g"
                , "NOT x -> h"
                , "NOT y -> i"
                ]

tests = test [ {-"sample d" ~: Just 72 ~=? signalOnWire "d" sampleCircuit
             , "sample e" ~: Just 72 ~=? signalOnWire "e" sampleCircuit
             , "sample f" ~: Just 72 ~=? signalOnWire "f" sampleCircuit
             , "sample g" ~: Just 72 ~=? signalOnWire "g" sampleCircuit
             , "sample h" ~: Just 72 ~=? signalOnWire "h" sampleCircuit
             , "sample i" ~: Just 72 ~=? signalOnWire "i" sampleCircuit
             , "sample x" ~: Just 72 ~=? signalOnWire "x" sampleCircuit
             , "sample y" ~: Just 72 ~=? signalOnWire "y" sampleCircuit
             -}
             "parse const" ~: ConstSpec "x" 123 ~=? parseWire "123 -> x"
             , "parse and" ~: AndSpec "d" "x" "y" ~=? parseWire "x AND y -> d"
             , "parse or" ~: OrSpec "d" "x" "y" ~=? parseWire "x OR y -> d"
             , "parse lshift" ~: LShiftSpec "f" "x" 2 ~=?
                parseWire "x LSHIFT 2 -> f"
             , "parse rshift" ~: RShiftSpec "f" "x" 2 ~=?
                parseWire "x RSHIFT 2 -> f"
             , "parse not" ~: NotSpec "h" "x" ~=? parseWire "NOT x -> h"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
