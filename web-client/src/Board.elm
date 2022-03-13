module Board exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)

type Tile
  = Letter Char
  | Blank

tileToChar : Tile -> Char
tileToChar tile =
  case tile of
    Letter c -> c
    Blank -> ' '

tileOfChar : Char -> Maybe Tile
tileOfChar c =
  if Char.isAlpha c
  then Just (Letter (Char.toUpper c))
  else if c == ' '
  then Just Blank
  else Nothing

type alias TileData =
  { score : Int
  , count : Int
  }

type alias Rack = List Tile

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
