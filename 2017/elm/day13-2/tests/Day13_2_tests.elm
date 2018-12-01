module Day13_2_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
  describe "Day 13"
    [ describe "update"
      [ describe "Advance"
        [ test "moves each downward scanner down" <|
          \() ->
            let
              initialLayers = 
                [ { depth = 0, range = 2, scannerRange = 0, dir = Down }
                , { depth = 1, range = 3, scannerRange = 1, dir = Down }
                ]
              updated = update Advance (makeModel initialLayers)
            in
              Expect.equal (scannerPoses updated) [1, 2]
        , test "moves each upward scanner up" <|
          \() ->
            let
              initialLayers = [{ depth = 0, range = 2, scannerRange = 1, dir = Up }]
              updated = update Advance (makeModel initialLayers)
            in
              Expect.equal (scannerPoses updated) [0]
        , test "reverses scanner direction when it reaches bottom" <|
          \() ->
            let
              initialLayers = [{ depth = 0, range = 2, scannerRange = 1, dir = Down }]
              updated = update Advance (makeModel initialLayers)
              expected = [{ depth = 0, range = 2, scannerRange = 0, dir = Up }]
                
            in
              Expect.equal expected updated.layers
        , test "reverses scanner direction when it reaches top" <|
          \() ->
            let
              initialLayers = [{ depth = 0, range = 2, scannerRange = 0, dir = Up }]
              updated = update Advance (makeModel initialLayers)
              expected = [{ depth = 0, range = 2, scannerRange = 1, dir = Down }]
                
            in
              Expect.equal expected updated.layers
        , test "copes with a range of 1 going down" <|
          \() ->
            let
              initialLayers = [{ depth = 0, range = 1, scannerRange = 0, dir = Down }]
              updated = update Advance (makeModel initialLayers)
              
            in
              Expect.equal (scannerPoses updated) [0]
        , test "copes with a range of 1 going up" <|
          \() ->
            let
              initialLayers = [{ depth = 0, range = 1, scannerRange = 0, dir = Up }]
              updated = update Advance (makeModel initialLayers)
              
            in
              Expect.equal (scannerPoses updated) [0]
        , test "moves the player deeper" <|
          \() ->
            let
              initialLayers = [anyLayer 0, anyLayer 1]
              updated = update Advance (makeModel initialLayers)
            in
              Expect.equal 1 updated.playerDepth
        , test "tracks when the player is caught" <|
          \() ->
            let
              updated = advanceTimes 7 sampleModel
            in
              Expect.equal [6, 0] updated.caughtAt
        ]
      , describe "Finish"
        [ test "advances to the end" <|
          \() ->
            let
              result = update Finish sampleModel
            in
              Expect.equal [6, 0] result.caughtAt
        ]
      , describe "Delay handling"
        [ test "does not move the player for delay ticks" <|
          \() ->
            let
              initial = { sampleModel | delay = 2 }
              updated = advanceTimes 2 initial
            in
              Expect.equal -1 updated.playerDepth
        , test "moves the player after delay ticks" <|
          \() ->
            let
              initial = { sampleModel | delay = 2 }
              updated = advanceTimes 3 initial
            in
              Expect.equal 0 updated.playerDepth
        ]
      , describe "Auto solve"
        [ test "finds the lowest delay that avoids being caught" <|
          \() ->
            let
              result = update AutoSolve sampleModel
            in
              Expect.equal 10 result.delay
        ]
      ]
    , describe "advanceThroughDelay"
      [ test "stops after the delay (1)" <|
        \() ->
          let
            layer = { depth = 0, range = 3, scannerRange = 0, dir = Down}
            input = { layers = [layer]
                    , clock = 0
                    , delay = 1
                    , playerDepth = -1
                    , caughtAt = []
                    }
            result = advanceThroughDelay input
            expected = { input 
                       | layers = [{ layer | scannerRange = 1, dir = Down }]
                       , clock = 1
                       }
          in
            Expect.equal expected result
      , test "stops after the delay (2)" <|
        \() ->
          let
            layer = { depth = 0, range = 3, scannerRange = 0, dir = Down}
            input = { layers = [layer]
                    , clock = 0
                    , delay = 2
                    , playerDepth = -1
                    , caughtAt = []
                    }
            result = advanceThroughDelay input
            expected = { input 
                       | layers = [{ layer | scannerRange = 2, dir = Down }]
                       , clock = 2
                       }
          in
            Expect.equal expected result
      , test "stops after the delay (3)" <|
        \() ->
          let
            layer = { depth = 0, range = 3, scannerRange = 0, dir = Down}
            input = { layers = [layer]
                    , clock = 0
                    , delay = 3
                    , playerDepth = -1
                    , caughtAt = []
                    }
            result = advanceThroughDelay input
            expected = { input 
                       | layers = [{ layer | scannerRange = 1, dir = Up }]
                       , clock = 3
                       }
          in
            Expect.equal expected result
      ]
    ]

sampleModel : Model
sampleModel = 
  { layers = List.map makeLayer [(0, 3), (1, 2), (4, 4), (6, 4)]
  , clock = 0
  , delay = 0
  , playerDepth = -1
  , caughtAt = []
  }


anyLayer : Int -> Layer
anyLayer depth = { depth = depth, range = 1, scannerRange = 1, dir = Down }

scannerPoses : Model -> List Int
scannerPoses model = List.map (\(x) -> x.scannerRange) model.layers

advanceTimes : Int -> Model -> Model
advanceTimes times model =
  if times == 0 then
    model
  else
    advanceTimes (times - 1) (update Advance model)

makeModel : List Layer -> Model
makeModel layers =
  { layers = layers
  , delay = 0
  , clock = 0
  , playerDepth = 0
  , caughtAt = []
  }
