import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "^v" ~: 3 ~=? numHouses "^v"
             , "^>v<" ~: 3 ~=? numHouses "^>v<"
             , "^v^v^v^v^v" ~: 11 ~=? numHouses "^v^v^v^v^v"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
