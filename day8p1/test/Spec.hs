import Control.Monad
import System.Exit
import Test.HUnit

import Lib


tests = test [ "unescape end quotes" ~: "" ~=? unescape "\"\""
             , "unescape regular" ~: "abc" ~=? unescape "\"abc\""
             , "unescape escaped quotes" ~: "aaa\"a\"aa" ~=?
                unescape "aaa\\\"a\\\"aa"
             , "unescape escaped backslashes" ~: "a\\a" ~=? unescape "a\\\\a"
             , "unescapeHex" ~: "'" ~=? unescape "\\x27"
             , "delta" ~: 12 ~=? delta [ "\"\""
                                       , "\"abc\""
                                       , "\"aaa\\\"aaa\""
                                       , "\"\\x27\""
                                       ]
             ]

run = runTestTT tests

main :: IO ()
main = do
    counts2 <- run
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
