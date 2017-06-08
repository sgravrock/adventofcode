import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "hash" ~: "000001dbbfa3a5c83a2d506429c7b00e" ~=? hash "abcdef609043"
             , "goodHash y" ~: True ~=? goodHash "000001"
             , "goodHash n" ~: False ~=? goodHash "000011"
             , "abcdef" ~: 609043 ~=? nextCoin "abcdef" 0
             , "pqrstuv" ~: 1048970 ~=? nextCoin "pqrstuv" 0
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
