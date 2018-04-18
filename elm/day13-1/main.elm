module Main exposing (..)
import Html exposing (Html, table, tr, td, text, div, button)
import Html.Events exposing (onClick)

import Maybe

type Msg = Advance

type Direction = Down | Up

type alias Layer =
  { range: Int
  , scannerRange: Int
  , dir: Direction
  }

type alias Model = 
  { layers: List Layer
  , playerDepth: Int
  }

main =
  Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

model : Model
model = 
  { layers = List.map layerWithRange [3, 2, 4, 4]
  , playerDepth = -1
  }

layerWithRange: Int -> Layer
layerWithRange i = { range = i, scannerRange = 0, dir = Down }

update: Msg -> Model -> Model
update msg model = 
  case msg of
    Advance -> { model |
                 layers = List.map advanceScanner model.layers,
                 playerDepth = model.playerDepth + 1
               }

advanceScanner : Layer -> Layer
advanceScanner layer =
  if layer.range == 1 then
    layer
  else
    case layer.dir of
      Down -> 
        if layer.scannerRange + 1 < layer.range then
          { layer | scannerRange = layer.scannerRange + 1 }
        else
          advanceScanner { layer | dir = Up }
      Up ->
        if layer.scannerRange > 0 then
          { layer | scannerRange = layer.scannerRange - 1 }
        else
          advanceScanner { layer | dir = Down }

view : Model -> Html Msg
view model =
  div []
    [ table [] (bodyRows model 0 (numRows model))
    , button [ onClick Advance ] [ text "Advance" ]
    ]

bodyRows : Model -> Int -> Int -> List (Html Msg)
bodyRows model i max =
  if i < max then
    (bodyRow model i)::(bodyRows model (i + 1) max)
  else
    []

bodyRow : Model -> Int -> Html Msg
bodyRow model rowIx =
  let
    cells = List.indexedMap
      (\i  layer -> cell rowIx model.playerDepth i layer)
      model.layers
  in
    tr [] cells

cell : Int -> Int -> Int -> Layer -> Html Msg
cell rowIx playerDepth layerIx layer = td []
  [text (cellText rowIx playerDepth layerIx layer)]

cellText : Int -> Int -> Int -> Layer -> String
cellText rowIx playerDepth layerIx layer =
  if rowIx >= layer.range then
    ""
  else if rowIx == 0 && layer.scannerRange == 0 && playerDepth == layerIx then
    "(S)"
  else if rowIx == layer.scannerRange then
    "[S]"
  else if rowIx == 0 && playerDepth == layerIx then
    "( )"
  else
    "[ ]"

numRows : Model -> Int
numRows layers = 
  let
    ranges = List.map (\(layer) -> layer.range) model.layers
  in
    Maybe.withDefault 0 (List.maximum ranges)
