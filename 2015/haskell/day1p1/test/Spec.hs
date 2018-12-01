import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "(())" ~: 0 ~=? floorReached "(())"
             , "()()" ~: 0 ~=? floorReached "()()"
             , "(((" ~: 3 ~=? floorReached "((("
             , "(()(()(" ~: 3 ~=? floorReached "(()(()("
             , "))(((((" ~: 3 ~=? floorReached "))((((("
             , "())" ~: -1 ~=? floorReached "())"
             , ")))" ~: -3 ~=? floorReached ")))"
             , ")())())" ~: -3 ~=? floorReached ")())())"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
