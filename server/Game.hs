{-# LANGUAGE NamedFieldPuns #-}
module Game where

import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import Protocol

boardWidth, boardHeight :: Integer
boardWidth = 15
boardHeight = 15

isValidPos :: Pos -> Bool
isValidPos (Pos i j) = i >= 1 && j >= 1 && i <= boardHeight && j <= boardWidth

squareAt :: Pos -> Square
squareAt (Pos i j)
  | any isPos [(1, 1), (1, 8)] = emptySquare 1 3
  | any (\x -> isPos (x, x)) [2, 3, 4, 5, 8] = emptySquare 1 2
  | any isPos [(2, 6), (6, 6)] = emptySquare 3 1
  | any isPos [(1, 4), (3, 7), (4, 8), (7, 7)] = emptySquare 2 1
  | otherwise = emptySquare 1 1
  where
    emptySquare letterMult wordMult =
      Square { letterMult, wordMult, squareTile = Nothing }
    isPos (a, b) =
      let
        as = [a, boardHeight + 1 - a]
        bs = [b, boardWidth + 1 - b]
      in
      (i `elem` as && j `elem` bs) || (i `elem` bs && j `elem` as)


emptyBoard :: Board
emptyBoard =
  Board (Map.fromList (map (\p -> (p, squareAt p)) allPositions))
  where
    allPositions =
      [ Pos i j
      | i <- [1 .. boardHeight]
      , j <- [1 .. boardWidth]
      ]

applyMove :: Move -> Board -> Either MoveError Board
applyMove Move{ startPos, direction, tiles } (Board board)
  | null [() | PlaceTile _ <- tiles] = Left NoPlacedTiles
  | any (not . isValidPos . fst) posTiles = Left OffBoard
  | otherwise = Board <$> foldM applyTile board posTiles
  where
    goPos MoveRight n (Pos i j) = Pos i (j + n)
    goPos MoveDown  n (Pos i j) = Pos (i + n) j
    posTiles =
      [(goPos direction k startPos, moveTile) | (k, moveTile) <- zip [0..] tiles]
    updateSquare _ Nothing = Left OffBoard
    updateSquare UseBoard (Just sq) =
      case squareTile sq of
        Nothing -> Left TilesDoNotMatchBoard
        Just _ -> Right (Just sq)
    updateSquare (PlaceTile placed) (Just sq) =
      case squareTile sq of
        Just _ -> Left TilesDoNotMatchBoard
        Nothing -> Right (Just sq{ squareTile = Just placed })
    applyTile oldBoard (pos, moveTile) =
      Map.alterF (updateSquare moveTile) pos oldBoard

data GameState =
  GameState
    { folks :: Set Text
    , board :: Board
    }

newGame :: GameState
newGame = GameState{ folks = Set.empty, board = emptyBoard }
