module Main exposing (..)
import Html exposing (Html, table, tr, td, text, div, button)
import Html.Events exposing (onClick)

import Array
import Maybe

type Msg = Advance

type Direction = Down | Up

type alias Layer =
  { range: Int
  , scannerAt: Int
  , dir: Direction
  }

type alias Model = Array.Array Layer

main =
  Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

model : Model
model = List.map layerWithRange [3, 2, 4, 4] |> Array.fromList

layerWithRange: Int -> Layer
layerWithRange i = { range = i, scannerAt = 0, dir = Down }

update: Msg -> Model -> Model
update msg model = 
  case msg of
    Advance -> Array.map advanceScanner model

advanceScanner : Layer -> Layer
advanceScanner layer =
  if layer.range == 1 then
    layer
  else
    case layer.dir of
      Down -> 
        if layer.scannerAt + 1 < layer.range then
          { layer | scannerAt = layer.scannerAt + 1 }
        else
          advanceScanner { layer | dir = Up }
      Up ->
        if layer.scannerAt > 0 then
          { layer | scannerAt = layer.scannerAt - 1 }
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
  tr [] (List.map (\(layer) -> cell rowIx layer) (Array.toList model))

cell : Int -> Layer -> Html Msg
cell rowIx layer = td [] [text (cellText rowIx layer)]

cellText : Int -> Layer -> String
cellText rowIx layer =
  if rowIx >= layer.range then
    ""
  else if rowIx == layer.scannerAt then
    "[S]"
  else
    "[ ]"

numRows : Model -> Int
numRows layers = 
  let
    ranges = Array.map (\(layer) -> layer.range) model
  in
    Maybe.withDefault 0 (List.maximum (Array.toList ranges))
