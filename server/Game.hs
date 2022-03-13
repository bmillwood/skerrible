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

tileData :: Map.Map Tile TileData
tileData =
  fmap (\(tileScore, tileCount) -> TileData{ tileScore, tileCount })
  $ Map.fromList
    [ (Blank     , ( 0,  2))
    , (Letter 'A', ( 1,  9))
    , (Letter 'B', ( 3,  2))
    , (Letter 'C', ( 3,  2))
    , (Letter 'D', ( 2,  4))
    , (Letter 'E', ( 1, 12))
    , (Letter 'F', ( 4,  2))
    , (Letter 'G', ( 2,  3))
    , (Letter 'H', ( 4,  2))
    , (Letter 'I', ( 1,  9))
    , (Letter 'J', ( 8,  1))
    , (Letter 'K', ( 5,  1))
    , (Letter 'L', ( 1,  4))
    , (Letter 'M', ( 3,  2))
    , (Letter 'N', ( 1,  6))
    , (Letter 'O', ( 1,  8))
    , (Letter 'P', ( 3,  2))
    , (Letter 'Q', (10,  1))
    , (Letter 'R', ( 1,  6))
    , (Letter 'S', ( 1,  4))
    , (Letter 'T', ( 1,  6))
    , (Letter 'U', ( 1,  4))
    , (Letter 'V', ( 4,  2))
    , (Letter 'W', ( 4,  2))
    , (Letter 'X', ( 8,  1))
    , (Letter 'Y', ( 4,  2))
    , (Letter 'Z', (10,  1))
    ]
