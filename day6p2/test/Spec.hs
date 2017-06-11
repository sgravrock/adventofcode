import Control.Monad
import System.Exit
import Test.HUnit

import Lib
import qualified Data.Map.Strict as Map

oneOne = Map.singleton (Point 0 0) 1
oneTwo = Map.singleton (Point 0 0) 2

tests = test [ "parse on" ~: (Cmd On (XRange 887 959) (YRange 9 629)) ~=? parseCmd "turn on 887,9 through 959,629" 
             , "parse toggle" ~: (Cmd Toggle (XRange 887 959) (YRange 9 629)) ~=? parseCmd "toggle 887,9 through 959,629" 
             , "sumValues" ~: 6 ~=? sumValues (Map.fromList [('a',0),('b',1),('c',2),('d',3)])
             , "turnOn" ~: 3 ~=? sumValues (execute oneOne (Cmd On (XRange 0 1) (YRange 0 0)))
             , "turnOff" ~: 1 ~=? sumValues (execute oneTwo (Cmd Off (XRange 0 0) (YRange 0 0)))
             , "turnOff again" ~: 0 ~=? sumValues (execute Map.empty (Cmd Off (XRange 0 0) (YRange 0 0)))
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
