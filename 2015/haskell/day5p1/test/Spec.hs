import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "nice" ~: True ~=? isNice "ugknbfddgicrmopn"
             , "overlap" ~: True ~=? isNice "aaa"
             , "no double" ~: False ~=? isNice "jchzalrnumimnmhp"
             , "xy" ~: False ~=? isNice "haegwjzuvuyypxyu"
             , "ab" ~: False ~=? isNice "haegwjzuvuyypabu"
             , "cd" ~: False ~=? isNice "haegwjzuvuyypcdu"
             , "pq" ~: False ~=? isNice "haegwjzuvuyyppqu"
             , "vowels" ~: False ~=? isNice "advszwmarrgswjxmb"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
