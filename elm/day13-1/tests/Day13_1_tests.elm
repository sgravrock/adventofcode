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
            initialLayers = Array.fromList
              [ { range = 2, scannerAt = 0, dir = Down }
              , { range = 3, scannerAt = 1, dir = Down }
              ]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [1, 2]
      , test "moves each upward scanner up" <|
        \() ->
          let
            initialLayers = Array.fromList
              [ { range = 2, scannerAt = 1, dir = Up }
              ]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal (scannerPoses updated) [0]
      , test "reverses scanner direction when it reaches bottom" <|
        \() ->
          let
            initialLayers = Array.fromList [{ range = 2, scannerAt = 1, dir = Down }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            expected = Array.fromList [{ range = 2, scannerAt = 0, dir = Up }]
              
          in
            Expect.equal expected updated.layers
      , test "reverses scanner direction when it reaches top" <|
        \() ->
          let
            initialLayers = Array.fromList [{ range = 2, scannerAt = 0, dir = Up }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            expected = Array.fromList [{ range = 2, scannerAt = 1, dir = Down }]
              
          in
            Expect.equal expected updated.layers
      , test "copes with a range of 1 going down" <|
        \() ->
          let
            initialLayers = Array.fromList [{ range = 1, scannerAt = 0, dir = Down }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      , test "copes with a range of 1 going up" <|
        \() ->
          let
            initialLayers = Array.fromList [{ range = 1, scannerAt = 0, dir = Up }]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
            
          in
            Expect.equal (scannerPoses updated) [0]
      , test "moves the player deeper" <|
        \() ->
          let
            initialLayers = Array.fromList [anyLayer, anyLayer]
            initial = { layers = initialLayers, playerDepth = 0 }
            updated = update Advance initial
          in
            Expect.equal 1 updated.playerDepth
      ]
    ]

anyLayer : Layer
anyLayer = { range = 1, scannerAt = 1, dir = Down }

scannerPoses : Model -> List Int
scannerPoses model =
  let
    poses = Array.map (\(x) -> x.scannerAt) model.layers
  in
    Array.toList poses
