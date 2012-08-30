{-
  A solution to rubyquiz 27 (http://rubyquiz.com/quiz27.html).

  Given a standard 8 x 8 chessboard where each position is indicated in algebraic
  notation (with the lower left corner being a1), design a script that accepts
  two or more arguments.

  The first argument indicates the starting position of the knight. The second
  argument indicates the ending position of the knight. Any additional arguments
  indicate positions that are forbidden to the knight.

  Return an array indicating the shortest path that the knight must travel to
  get to the end position without landing on one of the forbidden squares.
  If there is no valid path to the destination return nil.

  Usage: ./KnightsTravails start_pos target_pos [blocked_pos]*

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}

module Main where

import qualified Data.Set as S
import AStar
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Arrow (second)
import System.Environment (getArgs)

-- A square on the chess board
type Square = (Int, Int)

-- A chess board with the knight's current position and a set of blocked squares
data Board = Board { knightPos :: Square, blockedSquares :: S.Set Square }
             deriving (Ord, Eq)

-- Converts a string in chess notation to a square. eg. a1 -> (1,1)
fromNotation :: String -> Square
fromNotation (x : y) = (fromJust (x `elemIndex` ['a'..'h']) + 1, read y)

-- Converts a square to a string in chess notation. eg. (1,1) -> a1
toNotation :: Square -> String
toNotation (x, y) = ((['a'..'h'] !! (x - 1)) : "") ++ show y

-- Checks if a string is a valid chess notation
isValidNotation notation =
  and [length notation == 2,
       head notation `elem` ['a'..'h'],
       last notation `elem` ['1'..'8']]

-- Makes Board an instance of SearchNode for astar to work
instance SearchNode Board Int where
  -- Finds the next possible board configurations for one knight's move.
  -- Move cost is one.
  nextNode board@(Board {..}) =
    zip
      (map (\pos -> board { knightPos = pos })
       . filter isValidMove
       . map (\(x, y) -> (fst knightPos + x, snd knightPos + y))
       $ moves)
      (repeat 1)
    where
      moves = [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]
      isValidMove (x, y) =
        and [x > 0, x < 9, y > 0, y < 9, not $ (x, y) `S.member` blockedSquares]

knightAstar heuristic blockedSquares start target =
  fmap (second (map knightPos))
  $ astar (Board start blockedSquares) (Board target blockedSquares) heuristic

-- Finds a path from a start square to an end square using BFS
bfsSearch :: S.Set Square -> Square -> Square -> Maybe (Int, [Square])
bfsSearch = knightAstar (\_ _ -> 0)

-- Finds a path from a start square to an end square using AStar with
-- half of the max of coordinate deltas as the heuristic
astarSearch :: S.Set Square -> Square -> Square -> Maybe (Int, [Square])
astarSearch =
  knightAstar (\(Board (x1,y1) _) (Board (x2,y2) _) ->
                  max (abs (x1-x2)) (abs (y1-y2)) `div` 2)

main = do
  args <- getArgs
  if length args < 2
  then error "Usage: ./KnightsTravails start_pos target_pos [blocked_pos]*"
  else if any (not . isValidNotation) args
       then error "Invalid board position"
       else let
         (start : target : blocked) = args
       in case astarSearch (S.fromList . map fromNotation $ blocked)
                           (fromNotation start) (fromNotation target) of
          Just (_, path) -> putStrLn . unwords . map toNotation $ path
          Nothing -> putStrLn "No path found"