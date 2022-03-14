{-# LANGUAGE NamedFieldPuns #-}
module Game where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Random as Random

import Protocol

boardWidth, boardHeight :: Integer
boardWidth = 15
boardHeight = 15

rackSize :: Integer
rackSize = 7

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

applyMoveToBoard :: Move -> Board -> Either MoveError Board
applyMoveToBoard Move{ startPos, direction, tiles } (Board board) =
  Board <$> foldM applyTile board posTiles
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

data PlayerState =
  PlayerState
    { rack :: Rack
    , score :: Integer
    }

data GameState =
  GameState
    { players :: Map Username PlayerState
    , board :: Board
    , bag :: Map Tile Integer
    , rng :: Random.StdGen
    }

createGame :: Random.StdGen -> GameState
createGame rng =
  GameState
    { players = Map.empty
    , board = emptyBoard
    , bag = fmap tileCount tileData
    , rng
    }

drawTile :: GameState -> Maybe (GameState, Tile)
drawTile game@GameState{ bag, rng } =
  case result of
    Left _ -> Nothing
    Right tile ->
      Just (game{ bag = decrement tile bag, rng = newRNG }, tile)
  where
    result =
      Map.foldlWithKey
        (\c tile tileC ->
          case c of
            Right _ -> c
            Left notEnough ->
              if notEnough + tileC >= ix
              then Right tile
              else Left (notEnough + tileC)
        )
        (Left 0)
        bag
    decrement key = Map.update (\x -> if x <= 1 then Nothing else Just (x - 1)) key
    (ix, newRNG) = Random.uniformR (0, numTiles - 1) rng
    numTiles = sum bag

drawTiles :: Integer -> GameState -> (GameState, [Tile])
drawTiles n game
  | n <= 0 = (game, [])
  | otherwise =
    case drawTile game of
      Nothing -> (game, [])
      Just (nextGame, tile) ->
        let (finalGame, rest) = drawTiles (n - 1) nextGame in
        (finalGame, tile : rest)

fillPlayerRack :: GameState -> PlayerState -> (GameState, PlayerState)
fillPlayerRack game pst@PlayerState{ rack = Rack tiles } =
  fmap
    (\drawn -> pst{ rack = Rack (tiles ++ drawn) })
    (drawTiles (rackSize - toInteger (length tiles)) game)

fillRack :: Username -> GameState -> GameState
fillRack username game@GameState{ players } =
    nextGame{ players = newPlayers }
  where
    (nextGame, newPlayers) =
      Map.alterF
        (maybe (game, Nothing) (fmap Just . fillPlayerRack game))
        username
        players

addPlayer :: Username -> GameState -> GameState
addPlayer username game@GameState{ players } =
  nextGame{ players = Map.insert username filledPlayer players }
  where
    (nextGame, filledPlayer) =
      fillPlayerRack game PlayerState{ rack = Rack [], score = 0 }

removePlayer :: Username -> GameState -> GameState
removePlayer username game =
  game{ players = Map.delete username (players game) }

takeFrom :: (Eq a) => [a] -> [a] -> Either (NonEmpty a) [a]
takeFrom [] right = Right right
takeFrom (x : xs) right =
  case break (== x) right of
    (_, []) ->
      case takeFrom xs right of
        Left others -> Left (NonEmpty.cons x others)
        Right _ -> Left (x :| [])
    (before, _ : after) -> takeFrom xs (before ++ after)

applyMove :: Username -> Move -> GameState -> Either MoveError GameState
applyMove username move@Move{ tiles = moveTiles } game@GameState{ board, players } = do
  tiles <-
    case [tile | PlaceTile tile <- moveTiles] of
      [] -> Left NoPlacedTiles
      tiles -> Right tiles
  Rack rackTiles <-
    case Map.lookup username players of
      Nothing -> Left NotPlaying
      Just PlayerState{ rack } -> Right rack
  newRackTiles <- first YouDoNotHave (takeFrom tiles rackTiles)
  let
    newPlayers = Map.adjust (\pst -> pst{ rack = Rack newRackTiles }) username players
  fmap
    (\newBoard -> fillRack username game{ board = newBoard, players = newPlayers })
    (applyMoveToBoard move board)

tileData :: Map Tile TileData
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
