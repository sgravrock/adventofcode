module Main exposing (..)
import Html exposing (Html, table, thead, tbody, th, tr, td, text, div, button)
import Html.Events exposing (onClick)

import Maybe

type Msg = Advance | Finish

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
  { layers = List.map makeLayer puzzleInput
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

isDone: Model -> Bool
isDone model = model.playerDepth == maxDepth model

layerAtDepth: Int -> List Layer -> Maybe Layer
layerAtDepth depth layers =
  case layers of
    [] -> Nothing
    f::r ->
      if f.depth == depth then
        Just f
      else
        layerAtDepth depth r

severity: Model -> Int
severity model =
  let
    sevOfLayer = \depth -> 
      case layerAtDepth depth model.layers of
        Just layer -> layer.range * depth
        Nothing -> Debug.crash "Can't have been caught at a non-layer"
    severities = List.map sevOfLayer model.caughtAt
  in
    List.sum severities


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
    Finish ->
      if isDone model then
        model
      else
        update Advance model |> update Finish

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
    [ firewallTable model
    , div [] [text (status model)]
    , if isDone model then
        text "Done."
      else
        div []
          [ button [onClick Advance] [ text "Advance" ]
          , button [onClick Finish] [text "Finish"]
          ]
    ]

firewallTable : Model -> Html Msg
firewallTable model =
  table [] 
    [ thead [] [(headerRow model)]
    , tbody [] (bodyRows model 0 (numRows model))
    ]

headerRow : Model -> Html Msg
headerRow model =
  let
    cellRange = List.range 0 (numCols model)
    cells = List.map (\i -> th [] [text (toString i)]) cellRange
  in
    tr [] cells

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
    "Caught at " ++ (toString (List.reverse model.caughtAt)) ++ ". Severity: " ++ (toString (severity model))


puzzleInput : List (Int, Int)
puzzleInput =
  [ (0, 5)
  , (1, 2)
  , (2, 3)
  , (4, 4)
  , (6, 6)
  , (8, 4)
  , (10, 6)
  , (12, 10)
  , (14, 6)
  , (16, 8)
  , (18, 6)
  , (20, 9)
  , (22, 8)
  , (24, 8)
  , (26, 8)
  , (28, 12)
  , (30, 12)
  , (32, 8)
  , (34, 8)
  , (36, 12)
  , (38, 14)
  , (40, 12)
  , (42, 10)
  , (44, 14)
  , (46, 12)
  , (48, 12)
  , (50, 24)
  , (52, 14)
  , (54, 12)
  , (56, 12)
  , (58, 14)
  , (60, 12)
  , (62, 14)
  , (64, 12)
  , (66, 14)
  , (68, 14)
  , (72, 14)
  , (74, 14)
  , (80, 14)
  , (82, 14)
  , (86, 14)
  , (90, 18)
  , (92, 17)
  ]
