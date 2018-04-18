module Day13_1_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
  describe "update"
    [ describe "Advance"
      [ test "moves each downward scanner down" <|
        \() ->
          let
            initialLayers = 
              [ { depth = 0, range = 2, scannerRange = 0, dir = Down }
              , { depth = 1, range = 3, scannerRange = 1, dir = Down }
              ]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [1, 2]
      , test "moves each upward scanner up" <|
        \() ->
          let
            initialLayers = [{ depth = 0, range = 2, scannerRange = 1, dir = Up }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [0]
      , test "reverses scanner direction when it reaches bottom" <|
        \() ->
          let
            initialLayers = [{ depth = 0, range = 2, scannerRange = 1, dir = Down }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            expected = [{ depth = 0, range = 2, scannerRange = 0, dir = Up }]
              
          in
            Expect.equal expected updated.layers
      , test "reverses scanner direction when it reaches top" <|
        \() ->
          let
            initialLayers = [{ depth = 0, range = 2, scannerRange = 0, dir = Up }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            expected = [{ depth = 0, range = 2, scannerRange = 1, dir = Down }]
              
          in
            Expect.equal expected updated.layers
      , test "copes with a range of 1 going down" <|
        \() ->
          let
            initialLayers = [{ depth = 0, range = 1, scannerRange = 0, dir = Down }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      , test "copes with a range of 1 going up" <|
        \() ->
          let
            initialLayers = [{ depth = 0, range = 1, scannerRange = 0, dir = Up }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      , test "moves the player deeper" <|
        \() ->
          let
            initialLayers = [anyLayer 0, anyLayer 1]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal 1 updated.playerDepth
      ]
    ]

anyLayer : Int -> Layer
anyLayer depth = { depth = depth, range = 1, scannerRange = 1, dir = Down }

scannerPoses : Model -> List Int
scannerPoses model = List.map (\(x) -> x.scannerRange) model.layers
