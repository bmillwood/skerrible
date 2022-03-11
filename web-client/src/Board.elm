module Board exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

type alias Tile =
  { char : Char
  , score : Int
  }

type alias Rack = List Tile

rackByChar : Rack -> Dict Char (Dict Int Int)
rackByChar =
  let
    addToByScore score =
      Just
      << Dict.update score (\count -> Just (1 + Maybe.withDefault 0 count))
      << Maybe.withDefault Dict.empty
  in
  List.foldl
    (\tile dict -> Dict.update tile.char (addToByScore tile.score) dict)
    Dict.empty

type alias Square =
  { letterMult : Int
  , wordMult : Int
  , tile : Maybe Tile
  }

emptySquare : Square
emptySquare = { letterMult = 1, wordMult = 1, tile = Nothing }

type alias Board =
  { top : Int
  , left : Int
  , squares : Array (Array Square)
  }

empty : Board
empty = { top = 0, left = 0, squares = Array.empty }

width : Board -> Int
width board = Array.foldl max 0 (Array.map Array.length board.squares)

height : Board -> Int
height board = Array.length board.squares

get : Int -> Int -> Board -> Maybe Square
get rowN colN { top, left, squares } =
  Array.get (rowN - top) squares
  |> Maybe.andThen (Array.get (colN - left))

getTile : Int -> Int -> Board -> Maybe Tile
getTile rowN colN board =
  get rowN colN board
  |> Maybe.andThen .tile
