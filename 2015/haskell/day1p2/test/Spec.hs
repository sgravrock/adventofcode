import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ ")" ~: 1 ~=? stepsToBasement ")"
             , "()())" ~: 5 ~=? stepsToBasement "()())"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
