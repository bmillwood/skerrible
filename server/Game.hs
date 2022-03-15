{-# LANGUAGE NamedFieldPuns #-}
module Game where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import qualified System.Random as Random

import Protocol

boardWidth, boardHeight :: Integer
boardWidth = 15
boardHeight = 15
boardCentre :: Pos
boardCentre = Pos (div (boardHeight + 1) 2) (div (boardWidth + 1) 2)

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

posInMoveAt :: Move -> Integer -> Pos
posInMoveAt Move{ startPos = Pos si sj, direction, tiles = _ } i =
  case direction of
    MoveRight -> Pos si (sj + i)
    MoveDown -> Pos (si + i) sj

placedPositionsForMove :: Move -> [Pos]
placedPositionsForMove move@Move{ startPos = _, direction = _, tiles } =
  [posInMoveAt move i | (i, PlaceTile _) <- zip [0 ..] tiles]

isMoveConnected :: Move -> Board -> Bool
isMoveConnected move (Board board) =
  any
    (\pos -> maybe False (isJust . squareTile) (Map.lookup pos board))
    neighbouringPositions
  where
    neighbouringPositions =
      concatMap
        (\(Pos i j) -> [Pos (i - 1) j, Pos (i + 1) j, Pos i (j - 1), Pos i (j + 1)])
        (placedPositionsForMove move)

noTilesPlayed :: Board -> Bool
noTilesPlayed (Board boardMap) = all (isNothing . squareTile) boardMap

connectednessCheck :: Move -> Board -> Either MoveError ()
connectednessCheck move board
  | noTilesPlayed board =
    if any (== boardCentre) (placedPositionsForMove move)
    then Right ()
    else Left FirstMoveNotInCentre
  | otherwise =
    if isMoveConnected move board
    then Right ()
    else Left DoesNotConnect

applyMoveToBoard :: Move -> Board -> Either MoveError Board
applyMoveToBoard move@Move{ startPos, direction, tiles } board@(Board boardMap) =
  connectednessCheck move board >> (Board <$> foldM applyTile boardMap posTiles)
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
    (ix, newRNG) = Random.randomR (0, numTiles - 1) rng
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

updatePlayer
  :: (GameState -> Maybe PlayerState -> (GameState, Maybe PlayerState))
  -> Username -> GameState -> GameState
updatePlayer f username game@GameState{ players } =
    nextGame{ players = newPlayers }
  where
    (nextGame, newPlayers) = Map.alterF (f game) username players

fillRack :: Username -> GameState -> GameState
fillRack =
  updatePlayer
    (\game -> maybe (game, Nothing) (fmap Just . fillPlayerRack game))

addPlayerIfAbsent :: Username -> GameState -> GameState
addPlayerIfAbsent = updatePlayer add
  where
    add game Nothing =
      fmap Just (fillPlayerRack game PlayerState{ rack = Rack [], score = 0 })
    add game (Just p) = (game, Just p)

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
