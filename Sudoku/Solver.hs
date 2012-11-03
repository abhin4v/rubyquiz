{-
 A solution to rubyquiz 43 (http://rubyquiz.com/quiz43.html).

 A fast multi-threaded Sudoku solver using recursive depth-first backtracking
 for searching and constraint propagation for solving.

 Solves the 49191 puzzles at http://school.maths.uwa.edu.au/~gordon/sudoku17
 in 32 seconds on a quad core machine with output switched off.

 Each puzzle should be formatted as a single line of 81 character, top to bottom,
 left to right, with digits 1 to 9 if the cell has a value else a dot (.).

 Example:
 4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......

 Usage:
  cat sudoku17 | bin/SudokuSolver +RTS -N4 -H800m -K50m
  echo "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......" | bin/SudokuSolver

 Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Sudoku.Solver (solveSudoku, main) where

import qualified Data.Set as S
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (newChan, writeChan, getChanContents)
import Control.Monad (forM_, forM)
import Data.Bits ((.&.), complement, bit)
import Data.List.Split (chunksOf)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Sudoku.Board

-- Solves a Sudoku board using recursive backtracking DFS.
solveSudoku :: Board -> Maybe Board
solveSudoku board
    -- if solved, return the board
  | isBoardSolved board     = Just board
    -- if no more unsolved cells left then return Nothing
  | S.null (ambCells board) = Nothing
    -- if the current cell has no possible values, solve with rest cells
  | val == 0                = solveSudoku $ board { ambCells = cs }
  | otherwise               = let
      -- create two cells from current cell, one with only the smallest possible
      -- value and second with the rest
      fs = bit . firstSol $ val
      nextPos = Cell ix fs 1
      restPos = Cell ix (val .&. complement fs) (vl - 1)
      boardR = updateBoard board restPos

      -- try to constrain with the current cell with only one value
      in case constrainBoard board nextPos of
           -- if failed, continue with the current cell with the rest values
           Nothing                       -> solveSudoku boardR
                         -- if solved, return the board
           Just board' | isBoardSolved board' -> Just board'
                         -- else try to recursively solve the board further
                       | otherwise            -> case solveSudoku board' of
                           -- if solved, return the board
                           Just board''       -> Just board''
                           -- else try to solve the board with the current cell
                           -- with the rest values
                           Nothing            -> solveSudoku boardR
  where
    -- Finds the cell which has fewest possible values.
    (Cell ix val vl, cs) = S.deleteFindMin (ambCells board)

-- Reads the puzzles from stdin and solves them
main :: IO ()
main = do
  -- read the puzzles in chunks of 100
  chunks <- fmap (chunksOf 100 . lines) getContents
  solChan <- newChan
  done <- newEmptyMVar

  -- print the solutions (or errors) and wait for all thread to finish
  forkIO $ do
    solutions <- getChanContents solChan
    printSolutions solutions (length chunks)
    putMVar done ()

  -- spawn a thread for each chunk
  forM_ chunks $ \chunk -> forkIO $ do
      -- for each line in the chunk, read it as a Sudoku board and solve it
      -- return solution as a string represented by showBoard if solvable else "Unsolvable"
      -- return an error if invalid board
      forM_ chunk $ \line -> do
        start <- getCPUTime
        let sudoku = readBoard line
        case sudoku of
          Nothing -> writeChan solChan $ "Invalid input sudoku: " ++ line
          Just board -> do
            let !res = solveSudoku board
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ^ 9) :: Double

            let !sol = printf "%s -> %s [%0.3f ms]" line
                         (maybe "Unsolvable" showBoard res) diff
            writeChan solChan sol
      writeChan solChan "DONE"

  takeMVar done
  where
    printSolutions _ 0           = return ()
    printSolutions ("DONE":xs) l = printSolutions xs (l - 1)
    printSolutions (x:xs) l      = putStrLn x >> printSolutions xs l
