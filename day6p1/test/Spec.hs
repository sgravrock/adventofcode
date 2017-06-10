import Control.Monad
import System.Exit
import Test.HUnit

import Lib
import qualified Data.Set as Set

setOfOrigin = Set.singleton (Point 0 0)
setOfTwo = Set.insert (Point 0 1) setOfOrigin
toggleResult = Set.fromList [(Point 0 1), (Point 1 1), (Point 1 0)]


tests = test ["run several commands" ~: 999999 ~=? runScript [
                "turn on 0,0 through 999,999"
              , "turn off 0,0 through 0,0"]
             , "parse on" ~: (Cmd On (XRange 887 959) (YRange 9 629)) ~=? parseCmd "turn on 887,9 through 959,629" 
             , "parse toggle" ~: (Cmd Toggle (XRange 887 959) (YRange 9 629)) ~=? parseCmd "toggle 887,9 through 959,629" 
             , "exec turn on 0-1,0-2" ~: 6 ~=? Set.size (execute Set.empty (Cmd On (XRange 0 1) (YRange 0 2)))
             , "exec turn off 0-0,1-1" ~: setOfOrigin ~=? execute setOfTwo (Cmd Off (XRange 0 0) (YRange 1 1))
             , "exec toggle 0-0,1-1" ~: toggleResult ~=? execute setOfOrigin (Cmd Toggle (XRange 0 1) (YRange 0 1))
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
