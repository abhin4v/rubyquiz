{-
  A solution to rubyquiz 31 (http://rubyquiz.com/quiz31.html).

  Generate a rectangular maze given the width and height. The maze should be
  solvable for any start and end position and there should be one possible
  solution a given pair of start and end positions.

  Generate an ASCII output representing the maze.

  Given the maze produced, find the solution. Produce ASCII output to visualize the solution.

  Usage: ./AmazingMazes <width> <height> <start_x> <start_y> <end_x> <end_y>
          Coordinates are zero based.

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE BangPatterns, TupleSections #-}

module Main where

import qualified Data.Map as M
import AStar
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Control.Monad.State (State, get, put, evalState)
import System.Environment (getArgs)
import System.Random (Random, StdGen, randomR, randomRs, newStdGen, split)

sliding :: Int -> Int -> [a] -> [[a]]
sliding _ _ [] = []
sliding size step xs = take size xs : sliding size step (drop step xs)

-- randomness --

type RandomState = State StdGen

getRandomR :: Random a => (a, a) -> RandomState a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

getRandomRs :: Random a => (a, a) -> RandomState [a]
getRandomRs limits = do
  gen <- get
  return $ randomRs limits gen

randomShuffle :: [a] -> RandomState [a]
randomShuffle list = do
  let len = length list
  rs <- getRandomRs (0, len - 1)
  g <- get
  let (_, g') = split g
  put g'
  return $ map (list !!) . head . dropWhile ((/= len) . length) . map nub . sliding len 1 $ rs

-- maze --

-- a cell with x and y coordinates
type Cell = (Int, Int)

-- a maze with width, height and a map of cell paths
data Maze = Maze Int Int (M.Map Cell [Cell]) deriving (Show)

-- a solution to a maze with the start and end cells and the path map
data MazeSolution = MazeSolution Cell Cell (M.Map Cell Cell)

-- get the neighbour cells
nextCells :: Int -> Int -> Cell -> [Cell]
nextCells width height (x, y) =
  filter (\(x', y') -> and [x' >= 0, x' < width, y' >= 0, y' < height])
  . map (\(xd, yd) -> (x + xd, y + yd))
  $ [(0,-1), (1,0), (0,1), (-1,0)]

-- generate a random maze given the start cell and empty maze
generateMaze_ :: Cell -> Maze -> RandomState Maze
generateMaze_ start maze@(Maze width height cellMap) = do
  !next <- randomShuffle . filter (not . flip M.member cellMap) $ nextCells width height start
  if null next
    then return $ Maze width height (M.insertWith' (++) start [] cellMap)
    else
      foldM (\mz@(Maze _ _ m) n -> (M.keys m) `seq`
              if not . M.member n $ m
                then generateMaze_ n
                      (Maze width height
                        (M.insertWith' (++) n [start] (M.insertWith' (++) start [n] m)))
                else return mz)
            maze next

-- generate a random maze given the maze width and height
generateMaze :: Int -> Int -> RandomState Maze
generateMaze width height = do
  x <- getRandomR (0, width - 1)
  y <- getRandomR (0, height - 1)
  generateMaze_ (x, y) (Maze width height M.empty)

-- render a maze and its solution as a string
renderMaze :: Maze -> MazeSolution -> String
renderMaze maze@(Maze width height _) solution =
  concatMap (renderMazeRow maze solution) [0 .. (height - 1)]
  ++ concat (replicate width "+---") ++ "+"

-- render a row of a maze and the maze's solution as a string
renderMazeRow :: Maze -> MazeSolution -> Int -> String
renderMazeRow maze@(Maze width height _) solution rowIx =
  let (up, side) = unzip . map (renderMazeCell maze solution rowIx) $ [0 .. (width - 1)]
  in concat up ++ "+" ++ "\n" ++ concat side ++ "|" ++ "\n"

-- render a cell of a maze and the maze's solution as a pair of strings
renderMazeCell :: Maze -> MazeSolution -> Int -> Int -> (String, String)
renderMazeCell (Maze _ _ cellMap) (MazeSolution start end solution) rowIx colIx = let
  cell = (colIx, rowIx)
  up = (colIx, rowIx - 1)
  side = (colIx - 1, rowIx)

  in ("+" ++ if up `elem` next cell then "   " else "---",
      (if side `elem` next cell then " " else "|") ++ " " ++ mark cell ++ " ")
  where
    next = fromMaybe [] . flip M.lookup cellMap
    mark cell@(x, y)
      | cell == start = "s"
      | cell == end   = "e"
      | otherwise = case M.lookup cell solution of
                      Nothing -> " "
                      Just (x', y') -> fromMaybe " " $ M.lookup (x' - x, y' - y) marks

-- symbols to mark the solution path
marks = M.fromList [((0,-1), "↑"), ((1,0), "→"), ((0,1), "↓"), ((-1,0), "←")]

-- solve the maze using astar give the maze and the start and end cells
solveMaze :: Maze -> Cell -> Cell -> MazeSolution
solveMaze maze@(Maze _ _ cellMap) start end =
  MazeSolution start end
  . M.fromList
  . map (\a -> (a !! 0, a !! 1))
  . filter ((== 2) . length)
  . sliding 2 1
  . fromMaybe [] . fmap snd
  . astar start end (map ((,1)) . fromMaybe [] . flip M.lookup cellMap)
  $ (\(x, y) (x', y') -> abs (x - x') + abs (y - y'))

main = do
  (width : height : sx : sy : ex : ey : _) <- fmap (map read) getArgs
  g <- newStdGen
  let mz = evalState (generateMaze width height) g
  putStrLn $ renderMaze mz (solveMaze mz (sx, sy) (ex, ey))