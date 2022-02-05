{-# LANGUAGE NamedFieldPuns #-}
module Game where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import Protocol

boardWidth, boardHeight :: Integer
boardWidth = 15
boardHeight = 15

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
      | i <- [1 .. boardWidth]
      , j <- [1 .. boardHeight]
      ]

data GameState =
  GameState
    { folks :: Set Text
    , board :: Board
    }

newGame :: GameState
newGame = GameState{ folks = Set.empty, board = emptyBoard }
