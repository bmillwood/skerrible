{-# LANGUAGE NamedFieldPuns #-}
import Data.Char
import qualified Data.Map as Map
import Test.HUnit

import Protocol
import Game

tileOfChar :: Char -> Maybe Tile
tileOfChar c
  | c == ' ' = Just Blank
  | isUpper c = Just (Letter c)
  | otherwise = Nothing

moveTiles :: [Char] -> [MoveTile]
moveTiles = map (moveTile . tileOfChar)
  where
    moveTile Nothing = UseBoard
    moveTile (Just t) = PlaceTile t

normalNewBoard :: Board
normalNewBoard = newBoard RoomSettings{ noBoardMultipliers = False }

testBoard :: Pos -> [String] -> Board
testBoard (Pos startRow startCol) rows =
  Board (foldl updateRow newBoardMap (zip [startRow ..] rows))
  where
    Board newBoardMap = normalNewBoard
    updateRow board (rowN, row) =
      foldl (update rowN) board (zip [startCol ..] row)
    update rowN board (colN, letter) =
      Map.adjust (updateSquare letter) (Pos rowN colN) board
    updateSquare letter sq@Square{ squareTile } =
      sq{ squareTile = maybe squareTile Just (tileOfChar letter) }

testMoveScore :: String -> Integer -> Move -> Board -> Test
testMoveScore prefix expectedScore move@Move{ tiles } board =
  TestCase
  $ assertEqual (prefix ++ map charOfTile tiles) expectedScore
  $ snd (scoreMove move board)
  where
    charOfTile UseBoard = '_'
    charOfTile (PlaceTile Blank) = ' '
    charOfTile (PlaceTile (Letter c)) = c

moveScores :: [(Integer, Move)] -> Test
moveScores moves = TestList finalTests
  where
    (finalTests, _) =
      foldl testMove ([], normalNewBoard) (zip [(1 :: Integer) ..] moves)
    testMove (tests, board) (i, (expectedScore, move)) =
      ( scoreTest : moveTest : tests
      , case moveResult of
          Right nextBoard -> nextBoard
          Left _ ->
            -- This is pretty silly because it means failures are likely to compound.
            -- But we care more about testing scores anyway.
            board
      )
      where
        moveResult = applyMoveToBoard move board
        scoreTest = testMoveScore (show i ++ ". ") expectedScore move board
        moveTest =
          TestCase
          $ case moveResult of
              Right _ -> return ()
              Left moveError -> assertFailure (show moveError)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ "moveScores" ~: moveScores
      [ ( 16, Move (Pos  8  7) MoveRight (moveTiles "PHI") )
      , (  7, Move (Pos  9  8) MoveDown  (moveTiles "ATE") )
      , ( 14, Move (Pos  7  9) MoveDown  (moveTiles "F_N") )
      , (  4, Move (Pos  9  9) MoveRight (moveTiles "_ON") )
      , ( 12, Move (Pos 10 11) MoveDown  (moveTiles "OW") )
      , ( 13, Move (Pos 11 12) MoveDown  (moveTiles "EARL") )
      , ( 11, Move (Pos 12  8) MoveRight (moveTiles "D") )
      , ( 18, Move (Pos 11  5) MoveRight (moveTiles "COV") )
      , ( 17, Move (Pos  8  5) MoveDown  (moveTiles "JUI_Y") )
      ]
  , "explicit scenario" ~:
      testMoveScore "bridge " 2
        (Move (Pos 8 5) MoveRight (moveTiles "  "))
        $ testBoard (Pos 7 4)
            [ "A..L"
            , "R..E"
            , "M..G"
            ]
  ]
