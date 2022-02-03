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
  | isPos 1 1 || isPos 1 8 || isPos 8 1 = emptySquare 1 3
  | isPos 8 8 = emptySquare 1 2
  | otherwise = emptySquare 1 1
  where
    emptySquare letterMult wordMult =
      Square { letterMult, wordMult, squareTile = Nothing }
    isPos a b =
      i `elem` [a, boardHeight + 1 - a]
      && j `elem` [b, boardWidth + 1 - b]

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
