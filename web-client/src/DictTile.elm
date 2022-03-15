module DictTile exposing (..)

import Dict exposing (Dict)

import Board

type alias DictTile v = Dict Char v

empty : DictTile v
empty = Dict.empty

fromList : List (Board.Tile, v) -> DictTile v
fromList =
  List.map (\(b, v) -> (Board.tileToChar b, v))
  >> Dict.fromList

get : Board.Tile -> DictTile v -> Maybe v
get t d = Dict.get (Board.tileToChar t) d
