import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ ">" ~: 2 ~=? numHouses ">"
             , "^>v<" ~: 4 ~=? numHouses "^>v<"
             , "^v^v^v^v^v" ~: 2 ~=? numHouses "^v^v^v^v^v"
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
