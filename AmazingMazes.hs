{-|
  A solution to rubyquiz 31 (<http://rubyquiz.com/quiz31.html>).

  /Generate a rectangular maze given its width and height. The maze should be/
  /solvable for any start and end positions and there should be only one possible/
  /solution for any pair of start and end positions./

  /Generate the ASCII output representing the maze./

  /Find the solution of the maze. Produce ASCII output to visualize the solution./

  The maze generation algorithm used is recursive backtracking and the maze
  solution algorithm used is A* (from "AStar" module).

  Usage (Coordinates are zero based):

  > ./AmazingMazes <width> <height> <start_x> <start_y> <end_x> <end_y>

  Example:

  > abhinav@xj9:rubyquiz# bin/AmazingMazes 10 10 0 0 9 9
  > +---+---+---+---+---+---+---+---+---+---+
  > | s   >   v         |           |       |
  > +---+---+   +---+   +   +---+   +   +---+
  > | v   <   < |       |       |   |   |   |
  > +   +---+---+---+---+---+   +   +   +   +
  > | v                 |       |       |   |
  > +   +---+---+---+   +   +---+---+---+   +
  > | >   >   >   v |   |   |         >   v |
  > +---+---+---+   +   +   +---+---+   +   +
  > |       | v   < |       | >   >   ^ | v |
  > +   +   +   +---+---+---+   +---+---+   +
  > |   |   | >   >   >   >   ^ |       | v |
  > +   +---+---+---+---+---+---+   +---+   +
  > |   |       |               | v   <   < |
  > +   +   +   +   +   +   +---+   +---+---+
  > |   |   |       |   |   | v   < |       |
  > +   +   +---+---+   +---+   +---+   +---+
  > |       |       |   | v   < |           |
  > +   +---+   +   +   +   +---+---+---+   +
  > |           |   |     >   >   >   >   e |
  > +---+---+---+---+---+---+---+---+---+---+

  Copyright 2012 Abhinav Sarkar \<abhinav\@abhinavsarkar.net\>
-}
{-# LANGUAGE BangPatterns, TupleSections #-}

module AmazingMazes (Cell(..), Maze(..), MazeSolution(..),
                     generateMaze, renderMaze, solveMaze, main)
where

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

-- | A cell with x and y coordinates
type Cell = (Int, Int)

-- | A maze with width, height and a map of cell paths
data Maze = Maze Int Int (M.Map Cell [Cell])

-- | A solution to a maze with the start and end cells and the path map
data MazeSolution = MazeSolution Cell Cell (M.Map Cell Cell)

-- | Gets the neighbour cells
nextCells :: Int -> Int -> Cell -> [Cell]
nextCells width height (x, y) =
  filter (\(x', y') -> and [x' >= 0, x' < width, y' >= 0, y' < height])
  . map (\(xd, yd) -> (x + xd, y + yd))
  $ [(0,-1), (1,0), (0,1), (-1,0)]

-- | Generates a random maze given the start cell and an empty maze
generateMaze_ :: Cell -> Maze -> RandomState Maze
generateMaze_ start maze@(Maze width height cellMap) = do
  !next <- randomShuffle . filter (not . flip M.member cellMap) $ nextCells width height start
  if null next
    then return $ Maze width height (M.insertWith' (++) start [] cellMap)
    else
      foldM (\mz@(Maze _ _ m) n -> M.keys m `seq`
              if not . M.member n $ m
                then generateMaze_ n
                      (Maze width height
                        (M.insertWith' (++) n [start] (M.insertWith' (++) start [n] m)))
                else return mz)
            maze next

-- | Generates a random maze given the maze width and height using recursive backtracking
generateMaze :: Int                  -- ^ Maze width
                -> Int               -- ^ Maze height
                -> State StdGen Maze -- ^ The generated maze inside a 'State' monad with a random generator
generateMaze width height = do
  x <- getRandomR (0, width - 1)
  y <- getRandomR (0, height - 1)
  generateMaze_ (x, y) (Maze width height M.empty)

-- | Renders a maze and its solution as a string
renderMaze :: Maze -> MazeSolution -> String
renderMaze maze@(Maze width height _) solution =
  concatMap (renderMazeRow maze solution) [0 .. (height - 1)]
  ++ concat (replicate width "+---") ++ "+"

-- | Renders a row of a maze and the maze's solution as a string
renderMazeRow :: Maze -> MazeSolution -> Int -> String
renderMazeRow maze@(Maze width height _) solution rowIx =
  let (up, side) = unzip . map (renderMazeCell maze solution rowIx) $ [0 .. (width - 1)]
  in concat up ++ "+" ++ "\n" ++ concat side ++ "|" ++ "\n"

-- | Renders a cell of a maze and the maze's solution as a pair of strings
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

-- | Symbols to mark the solution path
marks = M.fromList [((0,-1), "^"), ((1,0), ">"), ((0,1), "v"), ((-1,0), "<")]

-- | Solves the maze using A* given the maze and the start and end cells using
-- Manhattan distance as the heuristic
solveMaze :: Maze            -- ^ The maze to solve
             -> Cell         -- ^ The start cell
             -> Cell         -- ^ The end cell
             -> MazeSolution -- ^ The solution of the maze
solveMaze maze@(Maze _ _ cellMap) start end =
  MazeSolution start end
  . M.fromList
  . map (\a -> (a !! 0, a !! 1))
  . filter ((== 2) . length)
  . sliding 2 1
  . fromMaybe [] . fmap snd
  . astar start end (map (,1) . fromMaybe [] . flip M.lookup cellMap)
  $ (\(x, y) (x', y') -> abs (x - x') + abs (y - y'))

-- | Reads the width, height, start and end cell co-ordinates from command
-- line arguments, generates a maze using them, solves it and renders it
-- with the solution.
main = do
  (width : height : sx : sy : ex : ey : _) <- fmap (map read) getArgs
  g <- newStdGen
  let mz = evalState (generateMaze width height) g
  putStrLn $ renderMaze mz (solveMaze mz (sx, sy) (ex, ey))