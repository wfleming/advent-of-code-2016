module Main (main) where

import System.Environment (getArgs)
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String
import Text.Printf

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile parseMovements file
  let movements = either (error . show) id parseResult
  let finalPos = walk (0,0) movements
  putStrLn $ printf "Walked to " ++ show finalPos ++ " total distance = " ++ show (dist (0,0) finalPos)
  let hitTwice = fst $ hitTwice ((0,0), North) movements []
  putStrLn $ printf "first loc seen twice is " ++ show hitTwice ++ " total distance = " ++ show (dist (0,0) hitTwice)

type Point = (Int, Int)
data Movement = TurnRight Int | TurnLeft Int
data Orientation = North | South | West | East

walk :: Point -> [Movement] -> Point
walk start moves = fst $ foldl step (start, North) moves

hitTwice :: (Point, Orientation) -> [Movement] -> [Movement] -> Point
hitTwice (start, facing) moves placesBeen =
    case elem start moves of
        True -> start
        False -> hitTwice (step (start, facing) nextMove) restMoves [start]
  where
    nextMove : restMoves = moves

step :: (Point, Orientation) -> Movement -> (Point, Orientation)
step (pos, facing) move =
    (newPos, newDir)
  where
    newDir = turn facing move
    moveDist = extractMoveDist move
    newPos = performMove pos newDir moveDist

    extractMoveDist (TurnRight d) = d
    extractMoveDist (TurnLeft d) = d

    turn North (TurnRight _) = East
    turn North (TurnLeft _) = West
    turn South (TurnRight _) = West
    turn South (TurnLeft _) = East
    turn West (TurnRight _) = North
    turn West (TurnLeft _) = South
    turn East (TurnRight _) = South
    turn East (TurnLeft _) = North

    performMove (x, y) North d = (x, y + d)
    performMove (x, y) South d = performMove (x, y) North (0 - d)
    performMove (x, y) East d = (x + d, y)
    performMove (x, y) West d = performMove (x, y) East (0 - d)

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs(x2 - x1) + abs(y2 - y1)

-- Input is a number of movements separated by spaces
parseMovements :: Parser [Movement]
parseMovements = parseMovement `sepBy` (string ", ")

-- A movement is a direction & an integral distance
parseMovement :: Parser Movement
parseMovement = choice
    [ movementParser 'R' TurnRight
    , movementParser 'L' TurnLeft
    ]

movementParser :: Char -> (Int -> Movement) -> Parser Movement
movementParser dirToken ctor = do
    _ <- char dirToken
    return ctor <*> parseDistance

parseDistance :: Parser Int
parseDistance = read <$> many1 digit
