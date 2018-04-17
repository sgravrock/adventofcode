module Day13_1_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Array
import Main exposing (..)


suite : Test
suite =
  describe "update"
    [ describe "Advance"
      [ test "moves each downward scanner down" <|
        \() ->
          let
            initial = Array.fromList
              [ { range = 2, scannerAt = 0, dir = Down }
              , { range = 3, scannerAt = 1, dir = Down }
              ]
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [1, 2]
      , test "moves each upward scanner up" <|
        \() ->
          let
            initial = Array.fromList
              [ { range = 2, scannerAt = 1, dir = Up }
              ]
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [0]
      , test "reverses scanner direction when it reaches bottom" <|
        \() ->
          let
            initial = Array.fromList [{ range = 2, scannerAt = 1, dir = Down }]
            updated = update Advance initial
            expected = Array.fromList [{ range = 2, scannerAt = 0, dir = Up }]
              
          in
            Expect.equal expected updated
      , test "reverses scanner direction when it reaches top" <|
        \() ->
          let
            initial = Array.fromList [{ range = 2, scannerAt = 0, dir = Up }]
            updated = update Advance initial
            expected = Array.fromList [{ range = 2, scannerAt = 1, dir = Down }]
              
          in
            Expect.equal expected updated
      , test "copes with a range of 1 going down" <|
        \() ->
          let
            initial = Array.fromList [{ range = 1, scannerAt = 0, dir = Down }]
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      , test "copes with a range of 1 going up" <|
        \() ->
          let
            initial = Array.fromList [{ range = 1, scannerAt = 0, dir = Up }]
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      ]
    ]

scannerPoses : Model -> List Int
scannerPoses layers =
  let
    poses = Array.map (\(x) -> x.scannerAt) layers
  in
    Array.toList poses
