import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "2x3x4" ~: 58 ~=? wrappingPaper1 (2, 3, 4)
             , "1x1x10" ~: 43 ~=? wrappingPaper1 (1, 1, 10)
             , "sideAreas 2x3x4" ~: [6, 8, 12] ~=? sideAreas (2, 3, 4)
             , "smallestArea 2x3x4" ~: 6 ~=? smallestArea (2, 3, 4)
             , "parseLine 2x3x4" ~: (2, 3, 4) ~=? parseLine "2x3x4"
             , "wrappingPaper" ~: 101 ~=? wrappingPaper ["2x3x4", "1x1x10"]
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
