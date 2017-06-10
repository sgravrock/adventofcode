import Control.Monad
import System.Exit
import Test.HUnit

import Lib
import qualified Data.Set as Set


tests = test [-- "turn on 0,0 through 999,999" ~: 1000000 ~=? run "turn on 0,0 through 999,999"
             "exec turn on 0,1-0,2" ~: 6 ~=? Set.size (execute Set.empty (On (Range 0 1) (Range 0 1)))
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
