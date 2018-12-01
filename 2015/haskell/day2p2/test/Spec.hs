import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "2x3x4" ~: 34 ~=? ribbon1 (2, 3, 4)
             , "1x1x10" ~: 14 ~=? ribbon1 (1, 1, 10)
             , "parseLine 2x3x4" ~: (2, 3, 4) ~=? parseLine "2x3x4"
             , "ribbon" ~: 48 ~=? ribbon ["2x3x4", "1x1x10"]
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
