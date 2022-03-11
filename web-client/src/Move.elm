module Move exposing (..)

import Board

type Direction
  = Right
  | Down

type Tile
  = PlaceTile Board.Tile
  | UseBoard

type alias Move =
  { startRow : Int
  , startCol : Int
  , direction : Direction
  , tiles : List Tile
  }

posAt : Int -> Move -> (Int, Int)
posAt i { startRow, startCol, direction } =
  case direction of
    Right -> (startRow, startCol + i)
    Down -> (startRow + i, startCol)

nextPos : Move -> (Int, Int)
nextPos move = posAt (List.length move.tiles) move

type Error
  = NotPlaying
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | NoPlacedTiles
  | YouDoNotHave (List Board.Tile)
  | DoesNotConnect
  | NotAWord (List Move)
