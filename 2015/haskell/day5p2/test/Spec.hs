import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "nice" ~: True ~=? isNice "qjhvhtzxzqqjkmpb"
             , "nice 2" ~: True ~=? isNice "xxyxx"
             , "no triplet" ~: False ~=? isNice "uurcxstgmygtbstg"
             , "no pair" ~: False ~=? isNice "ieodomkazucvgmuy"
             , "hasRepeatingPair empty" ~: False ~=? hasRepeatingPair ""
             , "hasRepeatingPair y" ~: True ~=? hasRepeatingPair "aabaa"
             , "hasRepeatingPair no repeat" ~: False ~=? hasRepeatingPair "aaba"
             , "hasRepeatingPair overlap" ~: False ~=? hasRepeatingPair "aaa"
             , "hasRepeatingPair qjhvhtzxzqqjkmpb" ~: True ~=? hasRepeatingPair "qjhvhtzxzqqjkmpb"
             , "hasRepeatingPair uurcxstgmygtbstg" ~: True ~=? hasRepeatingPair "uurcxstgmygtbstg"
             , "hasTriplet aba" ~: True ~=? hasTriplet "aba"
             , "hasTriplet aaa" ~: True ~=? hasTriplet "aaa"
             , "hasTriplet aax" ~: False ~=? hasTriplet "aax"
             , "hasTriplet aa" ~: False ~=? hasTriplet "aa"
             , "hasTriplet a" ~: False ~=? hasTriplet "a"
             , "hasTriplet empty" ~: False ~=? hasTriplet ""
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
