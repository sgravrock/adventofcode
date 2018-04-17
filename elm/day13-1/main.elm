module Main exposing (..)
import Html exposing (Html, table, tr, td, text)
import Array
import Maybe

type Msg = NoOp

type alias Model = Array.Array Int -- of ranges

main =
  Html.beginnerProgram
  { model = model
  , view = view
  , update = update
  }

model : Model
model = Array.fromList [3, 2, 4, 4]

update: Msg -> Model -> Model
update msg model = model -- no-op for now

view : Model -> Html Msg
view model =
  table [] (bodyRows model 0 (numRows model))

bodyRows : Model -> Int -> Int -> List (Html Msg)
bodyRows model i max =
  if i < max then
    (bodyRow model i)::(bodyRows model (i + 1) max)
  else
    []

bodyRow : Model -> Int -> Html Msg
bodyRow model rowIx =
  tr [] (List.map (\(range) -> cell rowIx range) (Array.toList model))

cell : Int -> Int -> Html Msg
cell rowIx range =
  if rowIx < range then
    td [] [text "[ ]"]
  else
    td [] [text ""]

numRows : Model -> Int
numRows ranges = Maybe.withDefault 0 (List.maximum (Array.toList ranges))
