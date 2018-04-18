module Main exposing (..)
import Html exposing (Html, table, tr, td, text, div, button)
import Html.Events exposing (onClick)

import Maybe

type Msg = Advance

type Direction = Down | Up

type alias Layer =
  { range: Int
  , depth: Int
  , scannerRange: Int
  , dir: Direction
  }

type alias Model = 
  { layers: List Layer
  , playerDepth: Int
  , caughtAt: List Int
  }

main =
  Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

model : Model
model = 
  { layers = List.map makeLayer [(0, 3), (1, 2), (4, 4), (6, 4)]
  , playerDepth = -1
  , caughtAt = []
  }

makeLayer: (Int, Int) -> Layer
makeLayer (depth, range) =
  { range = range
  , depth = depth
  , scannerRange = 0
  , dir = Down
  }

maxDepth: Model -> Int
maxDepth model =
    case List.reverse model.layers |> List.head of
      Just deepest -> deepest.depth
      Nothing -> Debug.crash "Error: model has no layers"

layerAtDepth: Int -> List Layer -> Maybe Layer
layerAtDepth depth layers =
  case layers of
    [] -> Nothing
    f::r ->
      if f.depth == depth then
        Just f
      else
        layerAtDepth depth r


update: Msg -> Model -> Model
update msg model = 
  case msg of
    Advance ->
      let
        playerDepth = model.playerDepth + 1
        caughtAt =
          if caught playerDepth model.layers then
            playerDepth::model.caughtAt
          else
            model.caughtAt
        layers = List.map advanceScanner model.layers
      in
        { layers = layers, playerDepth = playerDepth, caughtAt = caughtAt }

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

caught : Int -> List Layer -> Bool
caught playerDepth layers =
  case layerAtDepth playerDepth layers of
    Nothing -> False
    Just layer -> layer.scannerRange == 0


view : Model -> Html Msg
view model =
  div []
    [ table [] (bodyRows model 0 (numRows model))
    , div [] [text (status model)]
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
    cellRange = List.range 0 (numCols model)
    cells = List.map (\i -> cell rowIx i model) cellRange
  in
    tr [] cells

cell : Int -> Int -> Model -> Html Msg
cell rowIx colIx model = td [] [text (cellText rowIx colIx model)]

cellText : Int -> Int -> Model -> String
cellText rowIx colIx model =
  case layerAtDepth colIx model.layers of
    Nothing -> 
      if rowIx > 0 then  
        ""
      else if model.playerDepth == colIx then
        "(.)"
      else
        "..."
    Just layer ->
      let
        playerHere = model.playerDepth == layer.depth
      in
        if rowIx >= layer.range then
          ""
        else if rowIx == 0 && layer.scannerRange == 0 && playerHere then
          "(S)"
        else if rowIx == layer.scannerRange then
          "[S]"
        else if rowIx == 0 && playerHere then
          "( )"
        else
          "[ ]"

numRows : Model -> Int
numRows layers = 
  let
    ranges = List.map (\(layer) -> layer.range) model.layers
  in
    Maybe.withDefault 0 (List.maximum ranges)

numCols : Model -> Int
numCols = maxDepth

status : Model -> String
status model =
  if model.caughtAt == [] then
    "OK so far"
  else
    "Caught at " ++ (toString (List.reverse model.caughtAt))
