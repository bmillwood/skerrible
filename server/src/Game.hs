{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Game where

import qualified Control.Lens as Lens
import Control.Monad (foldM, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Either (isRight)
import Data.Function
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Endo (Endo), appEndo)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
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

newBoard :: RoomSettings -> Board
newBoard RoomSettings{ noBoardMultipliers } =
  Board (Map.fromList (map (\p -> (p, squareAt p)) allPositions))
  where
    allPositions =
      [ Pos i j
      | i <- [1 .. boardHeight]
      , j <- [1 .. boardWidth]
      ]
    squareAt (Pos i j)
      | noBoardMultipliers = emptySquare 1 1
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

posInMoveAt :: Move -> Integer -> Pos
posInMoveAt Move{ startPos = Pos si sj, direction, tiles = _ } i =
  case direction of
    MoveRight -> Pos si (sj + i)
    MoveDown -> Pos (si + i) sj

placedPositionsForMove :: Move -> [(Pos, Tile)]
placedPositionsForMove move@Move{ startPos = _, direction = _, tiles } =
  [(posInMoveAt move i, tile) | (i, PlaceTile tile) <- zip [0 ..] tiles]

isMoveConnected :: Move -> Board -> Bool
isMoveConnected move (Board board) =
  any
    (\pos -> maybe False (isJust . squareTile) (Map.lookup pos board))
    neighbouringPositions
  where
    neighbouringPositions =
      concatMap
        (\(Pos i j, _) -> [Pos (i - 1) j, Pos (i + 1) j, Pos i (j - 1), Pos i (j + 1)])
        (placedPositionsForMove move)

noTilesPlayed :: Board -> Bool
noTilesPlayed (Board boardMap) = all (isNothing . squareTile) boardMap

connectednessCheck :: Move -> Board -> Either MoveError ()
connectednessCheck move board
  | noTilesPlayed board =
    if not (any (\(pos, _) -> pos == boardCentre) (placedPositionsForMove move))
    then Left FirstMoveNotInCentre
    else if null (drop 1 (tiles move))
    then
      -- Note that this is only a concern on the first move, since otherwise
      -- any connected move involves a multi-letter word.
      Left NoMultiletterWordsMade
    else Right ()
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

extendMove :: Board -> Move -> Move
extendMove (Board boardMap) move@Move{ startPos = _, direction, tiles } =
  Move{ startPos = extendedStartPos, direction, tiles = extendedTiles }
  where
    extendedEndTiles = tryExtendTiles (toInteger (length tiles)) []
    tryExtendTiles k tilesToAdd =
      case Map.lookup (posInMoveAt move k) boardMap of
        Just Square{ squareTile = Just _ } ->
          tryExtendTiles (k + 1) (UseBoard : tilesToAdd)
        _ -> tiles ++ tilesToAdd
    (extendedStartPos, extendedTiles) = tryExtendStartPos 0 extendedEndTiles
    tryExtendStartPos k tilesSoFar =
      case Map.lookup (posInMoveAt move (k - 1)) boardMap of
        Just Square{ squareTile = Just _ } ->
          tryExtendStartPos (k - 1) (UseBoard : tilesSoFar)
        _ -> (posInMoveAt move k, tilesSoFar)

crossMoves :: Board -> Move -> [Move]
crossMoves board move@Move{ direction } =
  filter
    (not . null . drop 1 . tiles)
    [ extendMove
        board
        Move
          { startPos = pos
          , direction = otherDirection direction
          , tiles = [PlaceTile tile]
          }
    | (pos, tile) <- placedPositionsForMove move
    ]
  where
    otherDirection MoveRight = MoveDown
    otherDirection MoveDown = MoveRight

wordOfMove :: Board -> Move -> String
wordOfMove (Board boardMap) move@Move{ tiles } =
    map (maybe '?' charOfTile . tileOfMoveTile move) (zip [0 ..] tiles)
  where
    tileOfMoveTile _ (_, PlaceTile tile) = Just tile
    tileOfMoveTile thisMove (i, UseBoard) =
      squareTile =<< Map.lookup (posInMoveAt thisMove i) boardMap
    charOfTile Blank = ' '
    charOfTile (Letter c) = c

scoreMove :: Move -> Board -> ([String], Integer)
scoreMove move@Move{ tiles } board@(Board boardMap) =
  ( map (wordOfMove board) allMoves
  , allTileBonus + sum (map scoreOneMove allMoves)
  )
  where
    allCrossMoves = crossMoves board move
    allTileBonus
      | toInteger (length [() | PlaceTile _ <- tiles]) == rackSize = 50
      | otherwise = 0
    extendedMove@Move{ tiles = extendedTiles } = extendMove board move
    allMoves =
      if null (drop 1 extendedTiles)
      then allCrossMoves
      else extendedMove : allCrossMoves
    scoreOneMove oneMove@Move{ tiles = oneTiles } =
      totalWordMult * sum [getScoreAt i tile | (i, tile) <- zip [0 ..] oneTiles]
      where
        getScoreAt i tile = getScore (Map.lookup (posInMoveAt oneMove i) boardMap) tile
        getScore sq (PlaceTile tile) = maybe 1 letterMult sq * getTileScore tile
        getScore sq UseBoard = maybe 0 (maybe 0 getTileScore . squareTile) sq
        totalWordMult =
          product
            [ wordMult
            | Just Square{ wordMult } <-
                map
                  (\(pos, _) -> Map.lookup pos boardMap)
                  (placedPositionsForMove oneMove)
            ]

data PlayerState
  = PlayerState
    { rack :: Rack
    , score :: Integer
    , consecutivePasses :: Integer
    }

newPlayer :: PlayerState
newPlayer = PlayerState{ rack = Rack [], score = 0, consecutivePasses = 0 }

data TurnState
  = NoTurns
  | Turns
      { prevMover :: Maybe Username
      , mustFollow :: Map Username Username
      , notMovedYet :: Set Username
      }

data GameState
  = GameState
    { players :: Map Username PlayerState
    , turns :: TurnState
    , board :: Board
    , gameOver :: Bool
    , bag :: Map Tile Integer
    , rng :: Random.StdGen
    }

data Game
  = Game
    { settings :: RoomSettings
    , gameHistory :: NonEmpty GameState
    }

latestState :: Game -> GameState
latestState Game{ gameHistory = x :| _ } = x

addState :: GameState -> Game -> Game
addState st game@Game{ gameHistory } =
  game{ gameHistory = NonEmpty.cons st gameHistory }

setLatestState :: GameState -> Game -> Game
setLatestState x game@Game{ gameHistory = _ :| xs } = game{ gameHistory = x :| xs }

undo :: Game -> Maybe Game
undo game@Game{ gameHistory = _ :| oldHistory } = do
  gameHistory <- NonEmpty.nonEmpty oldHistory
  return game{ gameHistory }

newGameState :: Set Username -> RoomSettings -> Random.StdGen -> GameState
newGameState players settings@RoomSettings{ turnEnforcement } rng =
  appEndo (foldMap (Endo . fillRack) players)
  $ GameState
    { players = Map.fromSet (const newPlayer) players
    , turns =
        case turnEnforcement of
          NoEnforcement ->
            NoTurns
          LetPlayersChoose ->
            Turns
              { prevMover = Nothing
              , mustFollow = Map.empty
              , notMovedYet = Set.empty
              }
    , board = newBoard settings
    , gameOver = False
    , bag = fmap tileCount tileData
    , rng
    }

createGame :: RoomSettings -> Random.StdGen -> Game
createGame settings rng =
  Game { settings, gameHistory = newGameState Set.empty settings rng :| [] }

startNewGame :: Game -> Game
startNewGame
  game@Game{ settings, gameHistory = history@(GameState { players, rng } :| _) }
  = game{ gameHistory = NonEmpty.cons newGame history }
  where
    newGame = newGameState (Map.keysSet players) settings rng

scores :: GameState -> Map Username Integer
scores GameState{ players } = Map.map score players

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
  :: (GameState -> Maybe PlayerState -> (r, Maybe PlayerState))
  -> Lens.Lens' r GameState
  -> Username -> GameState -> r
updatePlayer f rGS username game@GameState{ players } =
    r & Lens.over rGS (\nextGame -> nextGame{ players = newPlayers })
  where
    (r, newPlayers) = Map.alterF (f game) username players

fillRack :: Username -> GameState -> GameState
fillRack =
  updatePlayer
    (\game -> maybe (game, Nothing) (fmap Just . fillPlayerRack game))
    id

addPlayerIfAbsent :: Username -> GameState -> (PlayerState, GameState)
addPlayerIfAbsent username = updatePlayer add Lens._2 username
  where
    add game Nothing =
      ( case turns of
          NoTurns -> (p, nextGame)
          Turns{ notMovedYet } ->
            ( p
            , nextGame{ turns = turns{ notMovedYet = Set.insert username notMovedYet } }
            )
      , Just p
      )
      where
        (nextGame@GameState{ turns }, p) = fillPlayerRack game newPlayer
    add game (Just p) = ((p, game), Just p)

takeFrom :: (Eq a) => [a] -> [a] -> Either (NonEmpty a) [a]
takeFrom [] right = Right right
takeFrom (x : xs) right =
  case break (== x) right of
    (_, []) ->
      case takeFrom xs right of
        Left others -> Left (NonEmpty.cons x others)
        Right _ -> Left (x :| [])
    (before, _ : after) -> takeFrom xs (before ++ after)

removeFromPlayerRack
  :: [Tile]
  -> Username
  -> Map Username PlayerState
  -> Either MoveError (Map Username PlayerState)
removeFromPlayerRack tiles username players = do
  Map.alterF removeFromRack username players
  where
    removeFromRack Nothing = Left YouAreNotPlaying
    removeFromRack (Just pst@PlayerState{ rack = Rack rackTiles }) =
      case takeFrom tiles rackTiles of
        Left missing -> Left (YouDoNotHave missing)
        Right remaining -> Right (Just pst{ rack = Rack remaining })

applyGameEnd :: GameState -> GameState
applyGameEnd game@GameState{ players } =
  game{ gameOver = True, players = Map.map updateScoreFor players }
  where
    rackScore PlayerState{ rack = Rack tiles } = sum (map getTileScore tiles)
    unplayedTileScore = sum (Map.map rackScore players)
    updateScoreFor pst@PlayerState{ rack = Rack tiles, score }
      | null tiles = pst{ score = score + unplayedTileScore }
      | otherwise = pst{ score = score - rackScore pst }

endIfAnyRackEmpty :: GameState -> GameState
endIfAnyRackEmpty game@GameState{ players }
  | any (\PlayerState{ rack = Rack tiles } -> null tiles) players =
    applyGameEnd game
  | otherwise = game

advanceTurns :: Username -> TurnState -> Either MoveError TurnState
advanceTurns _ NoTurns = Right NoTurns
advanceTurns username Turns{ prevMover, mustFollow, notMovedYet } =
  case (prevMover, Map.lookup username mustFollow) of
    (Nothing, _) -> ok mustFollow
    (Just actual, Just expected)
      | expected /= actual -> Left NotYourTurn
      | otherwise -> ok mustFollow
    (Just prev, Nothing)
      | Set.member username notMovedYet || Set.null notMovedYet ->
          ok $ Map.insert username prev mustFollow
      | otherwise -> Left NotYourTurn
  where
    ok newFollow =
      Right Turns
        { prevMover = Just username
        , mustFollow = newFollow
        , notMovedYet = Set.delete username notMovedYet
        }

canMoveNext :: GameState -> Set Username
canMoveNext GameState{ players, turns } =
  Set.filter (\u -> isRight (advanceTurns u turns)) (Map.keysSet players)

takeTurn :: Username -> StateT GameState (Either MoveError) ()
takeTurn username = do
  game@GameState{ gameOver, players, turns } <- get
  when (not (Map.member username players)) $ lift $ Left YouAreNotPlaying
  when gameOver $ lift $ Left GameIsOver
  nextTurns <- lift $ advanceTurns username turns
  put game{ turns = nextTurns }

updatePassCountWith :: Username -> (Integer -> Integer) -> GameState -> GameState
updatePassCountWith username f game@GameState{ players } =
  game{ players = Map.update updatePassCount username players }
  where
    updatePassCount p@PlayerState{ consecutivePasses } =
      Just p{ consecutivePasses = f consecutivePasses }

applyMove :: Username -> Move -> GameState -> Either MoveError (MoveReport, GameState)
applyMove username move@Move{ tiles = moveTiles } = runStateT $ do
  takeTurn username
  tiles <-
    lift $ case [tile | PlaceTile tile <- moveTiles] of
      [] -> Left NoPlacedTiles
      tiles -> Right tiles
  game@GameState{ board, players } <- get
  unrackedPlayers <- lift $ removeFromPlayerRack tiles username players
  let
    (moveWords, moveScore) = scoreMove move board
    newPlayers =
      Map.adjust
        (\pst@PlayerState{ score } -> pst{ score = score + moveScore })
        username
        unrackedPlayers
  nextBoard <- lift $ applyMoveToBoard move board
  put
    $ endIfAnyRackEmpty
    $ fillRack username
    $ updatePassCountWith username (const 0)
      game{ board = nextBoard, players = newPlayers }
  return PlayedWord{ moveWords, moveScore }

returnToBag :: [Tile] -> GameState -> GameState
returnToBag tiles game@GameState{ bag } =
  game{ bag = foldl returnTile bag tiles }
  where
    returnTile acc tile = Map.insertWith (+) tile 1 acc

applyExchange
  :: Username
  -> NonEmpty Tile
  -> GameState
  -> Either MoveError (MoveReport, GameState)
applyExchange username tilesNE = runStateT $ do
  takeTurn username
  game@GameState{ bag, players } <- get
  when (toInteger (length tilesNE) > sum bag)
    $ lift $ Left NotEnoughTilesToExchange
  let tiles = NonEmpty.toList tilesNE
  nextPlayers <- lift $ removeFromPlayerRack tiles username players
  put
    $ returnToBag tiles
    $ fillRack username
    $ updatePassCountWith username (const 0)
    $ game{ players = nextPlayers }
  return $ Exchanged (toInteger $ length tiles)

applyPass :: Username -> GameState -> Either MoveError (MoveReport, GameState)
applyPass username = runStateT $ do
  takeTurn username
  modify $ updatePassCountWith username (+1)
  GameState{ players } <- get
  when
    (all (>= 2) [c | PlayerState{ consecutivePasses = c } <- Map.elems players])
    $ modify applyGameEnd
  return Passed

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

getTileScore :: Tile -> Integer
getTileScore tile = maybe 0 tileScore (Map.lookup tile tileData)
