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

module SudokuSolver (Cell(..), Board, emptyBoard, boardCells, cellValues,
                     isBoardSolved, readBoard, showBoard, prettyShowBoard,
                     solveSudoku, main)
where

import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (foldM, forM_, forM, (>=>))
import Data.Bits (testBit, (.&.), complement, popCount, bit)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (foldl', intersperse, intercalate, find, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Word (Word16)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- A cell in the Sudoku. The fields are cell index, possible cell values as
-- a bitset and number of possible cell values.
data Cell = Cell {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Word16
                 {-# UNPACK #-} !Int

-- The Sudoku board implemented as a HashMap from cell index to cell (ixMap).
-- ambCells is the set of cells which have not been solved yet.
data Board = Board { ixMap :: !(M.HashMap Int Cell),
                     ambCells :: !(S.Set Cell)
                   } deriving (Eq)

instance Eq Cell where
  {-# INLINE (==) #-}
  (Cell i1 v1 _) == (Cell i2 v2 _) = i1 == i2 && v1 == v2

instance Show Cell where
  show cell@(Cell ix val _) = "<" ++ show ix ++ " " ++ show (cellValues cell) ++ ">"

instance Ord Cell where
  (Cell i1 v1 vl1) `compare` (Cell i2 v2 vl2) =
    if i1 == i2 && v1 == v2
      then EQ
      else (vl1, i1) `compare`(vl2, i2)

cellValues :: Cell -> [Int]
cellValues (Cell _ val _) = filter (testBit val) [1..9]

boardCells :: Board -> [Cell]
boardCells = map snd . sortBy (comparing fst) . M.toList . ixMap

-- Gets the index of the lowest bit set as 1.
firstSol :: Word16 -> Int
firstSol val = fromJust . find (testBit val) $ [1..9]

cells :: Board -> [Int] -> [Cell]
cells board = map (fromJust . flip M.lookup (ixMap board))

rowIxs, columnIxs, boxIxs, unitIxs :: [[Int]]
rowIxs = chunksOf 9 [0..80]
columnIxs = map (\i -> take 9 [i, i + 9 ..]) [0..8]
boxIxs = concatMap (\(x:y:z:_) -> zipWith3 (\a b c -> a ++ b ++ c) x y z)
        . chunksOf 3 . map (chunksOf 3) $ rowIxs
unitIxs = rowIxs ++ columnIxs ++ boxIxs

-- Checks if a Sudoku board is solved.
-- A board is solved if all the cells have only one possible value and all rows,
-- columns and boxes follow the rule of Sudoku.
isBoardSolved :: Board -> Bool
isBoardSolved board =
  (all (\(Cell _ _ vl) -> vl == 1) . M.elems . ixMap $ board)
  && all (isUnitSolved . cells board) unitIxs
  where
    isUnitSolved unit = S.size (S.fromList unit) == 9

-- An empty Sudoku board where all cells have all possible values.
emptyBoard :: Board
emptyBoard =
  Board (foldl' (\m c@(Cell i _ _) -> M.insert i c m) M.empty cells)
        (S.fromList cells)
  where cells = map (\i -> Cell i 1022 9) [0..80]

-- Updates the given cell in the board.
updateBoard :: Board -> Cell -> Board
updateBoard board@Board{..} cell@(Cell ix _ vl) = case M.lookup ix ixMap of
  Nothing                        -> board
  Just oldCell | oldCell == cell -> board
               | vl == 1         -> Board (M.insert ix cell ixMap)
                                          (S.delete oldCell ambCells)
               | otherwise       -> Board (M.insert ix cell ixMap)
                                          (S.insert cell (S.delete oldCell ambCells))

-- Constrains the values of a cell (third argument) according to the values of
-- another cell (first argument) in the given board.
-- If there is a conflict in the values of the cells, returns Nothing.
constrainCell :: Cell -> Board -> Cell -> Maybe Board
constrainCell cell@(Cell _ val vl) board@Board{..} c@(Cell i pos pl)
  | c == cell            = return board
  | pos' == 0 && vl == 1 = Nothing
  | pos' == 0            = return board
  | pl' == 1 && pl > 1   = constrainBoard board (Cell i pos' pl')
  | otherwise            = return $ updateBoard board (Cell i pos' pl')
  where
    pos' = pos .&. complement val
    pl' = popCount pos'

-- Constrains the values of the given cells according to the values of the given
-- cell in the given board.
-- If there is a conflict in the values, returns Nothing.
constrainCells :: Cell -> Board -> [Cell] -> Maybe Board
constrainCells cell = foldM (constrainCell cell)

-- Constrains the given board according to the values of the given cell by
-- applying the rule of Sudoku: a unit cannot have same value for more than
-- one cell where a unit is a row, cell or a 3x3 box.
constrainBoard :: Board -> Cell -> Maybe Board
constrainBoard board cell@(Cell ix _ _) =
  foldM (\board' unitf -> constrainCells cell board' (unitf board'))
        (updateBoard board cell) [row, column, box]
  where
    (rowIx, colIx) = ix `divMod` 9
    (rowIx', colIx') = ((rowIx `div` 3) * 3, (colIx `div` 3) * 3)

    row board = cells board $ take 9 [rowIx * 9 ..]
    column board = cells board $ take 9 [colIx, colIx + 9 ..]
    box board =
      cells board [r * 9 + c | r <- [rowIx' .. rowIx' + 2], c <- [colIx' .. colIx' + 2]]

-- Reads a board from a properly formatted string.
-- Returns Nothing is the string represents an invalid board.
readBoard :: String -> Maybe Board
readBoard str =
  if length str /= 81
    then Nothing
    else foldM constrainBoard emptyBoard
        . map (\(ix, n) -> Cell ix (bit $ digitToInt n) 1)
        . filter (isDigit . snd)
        . zip [0..] $ str

-- Represents a board as a string of 81 characters in a single line with each
-- character being either a digit between 1 to 9 if there is a solution for
-- that cell else a dot (.).
showBoard :: Board -> String
showBoard board =
  zipWith (\(Cell _ val vl) dot ->
              if vl == 1 then intToDigit . firstSol $ val else dot)
          (boardCells board)
          (repeat '.')

-- Pretty prints a Sudoku board.
prettyShowBoard :: Board -> String
prettyShowBoard board =
  (\t -> line ++ "\n" ++ t ++ line ++ "\n")
  . unlines . intercalate [line] . chunksOf 3
  . map ((\r -> "| " ++ r ++ " |")
         . intercalate " | " . map (intersperse ' ') . chunksOf 3)
  . chunksOf 9
  . showBoard $ board
  where line = "+-------+-------+-------+"

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
  -- read the puzzles in chunks of 10
  chunks <- fmap (chunksOf 10 . lines) getContents

  -- spawn a thread for each chunk
  solutionsVs <- forM chunks $ \chunk -> do
    solutionsV <- newEmptyMVar
    forkIO $ do
      -- for each line in the chunk, read it as a Sudoku board and solve it
      -- return solution as a string represented by showBoard if solvable else "Unsolvable"
      -- return an error if invalid board
      solutions <- forM chunk $ \line -> do
        start <- getCPUTime
        let sudoku = readBoard line
        case sudoku of
          Nothing -> return $ "Invalid input sudoku: " ++ line
          Just board -> do
            let !res = solveSudoku board
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ^ 9) :: Double

            return $ printf "%s -> %s [%0.3f ms]" line
                       (maybe "Unsolvable" showBoard res) diff
      putMVar solutionsV solutions
    return solutionsV

  -- wait for all thread to finish and print the solutions (or errors)
  forM_ solutionsVs $ takeMVar >=> mapM_ putStrLn
