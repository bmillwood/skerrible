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

tileAtPos : Int -> Int -> Move -> Maybe Tile
tileAtPos rowN colN { startRow, startCol, direction, tiles } =
  case direction of
    Right ->
      if rowN == startRow && colN >= startCol
      then List.head (List.drop (colN - startCol) tiles)
      else Nothing
    Down ->
      if colN == startCol && rowN >= startRow
      then List.head (List.drop (rowN - startRow) tiles)
      else Nothing

remainingRack : Move -> Board.Rack -> Board.Rack
remainingRack move rack =
  let
    deleteFromList x xs =
      case xs of
        [] -> []
        y :: ys -> if x == y then ys else y :: deleteFromList x ys
  in
  List.foldl
    (\t a ->
      case t of
        UseBoard -> a
        PlaceTile tile -> deleteFromList tile a
    )
    rack
    move.tiles

type Error
  = NotPlaying
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | NoPlacedTiles
  | YouDoNotHave (List Board.Tile)
  | FirstMoveNotInCentre
  | DoesNotConnect
  | NotAWord (List Move)
