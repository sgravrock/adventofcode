import Control.Monad
import System.Exit
import Test.HUnit

import Lib

twoLevelInput = [ "jptl (61)"
                , "ugml (68) -> gyxo, jptl"
                , "gyxo (61)"
                ]
threeLevelInput = [ "pbga (66)"
                  , "xhth (57)"
                  , "ebii (61)"
                  , "havc (66)"
                  , "ktlj (57)"
                  , "fwft (72) -> ktlj, cntj, xhth"
                  , "qoyq (66)"
                  , "padx (45) -> pbga, havc, qoyq"
                  , "tknk (41) -> ugml, padx, fwft"
                  , "jptl (61)"
                  , "ugml (68) -> gyxo, ebii, jptl"
                  , "gyxo (61)"
                  , "cntj (57)"
                  ]

twoLevelSpec = [ NodeSpec { name="jptl", childNames=[] }
               , NodeSpec { name="ugml", childNames=["gyxo", "jptl"] }
               , NodeSpec { name="gyxo", childNames=[] }
               ]




tests = test [ "parse two level" ~: twoLevelSpec ~=? parseInput twoLevelInput
             , "two level root" ~: "ugml" ~=? root twoLevelSpec
             , "three level root" ~: "tknk" ~=? root (parseInput threeLevelInput)
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
