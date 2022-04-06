module Move exposing (..)

import Board

type Direction
  = Right
  | Down

type Tile
  = PlaceTile Board.Tile
  | UseBoard

placedTile : Tile -> Maybe Board.Tile
placedTile t =
  case t of
    UseBoard -> Nothing
    PlaceTile tile -> Just tile

type alias Move =
  { startRow : Int
  , startCol : Int
  , direction : Direction
  , tiles : List Tile
  }

type Proposal
  = ProposeMove Move
  | ProposeExchange (List Board.Tile)

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

diffList : List a -> List a -> List a
diffList xs ys =
  let
    deleteFromList y xsLeft =
      case xsLeft of
        [] -> []
        x :: rest -> if x == y then rest else x :: deleteFromList y rest
  in
  List.foldl deleteFromList xs ys

remainingRack : Proposal -> Board.Rack -> Board.Rack
remainingRack proposal rack =
  case proposal of
    ProposeMove move -> diffList rack (List.filterMap placedTile move.tiles)
    ProposeExchange tiles -> diffList rack tiles

type Error
  = YouAreNotPlaying
  | GameIsOver
  | NotYourTurn
  | OffBoard
  | TilesDoNotMatchBoard
  | NoPlacedTiles
  | YouDoNotHave (List Board.Tile)
  | FirstMoveNotInCentre
  | NoMultiletterWordsMade
  | DoesNotConnect
  | NotAWord (List Move)
  | NotEnoughTilesToExchange
