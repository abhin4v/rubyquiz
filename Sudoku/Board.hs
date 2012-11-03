{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Sudoku.Board (Cell(..), Board(..), emptyBoard, boardCells, cellValues,
                     firstSol, updateBoard, constrainBoard,
                     isBoardSolved, readBoard, showBoard, prettyShowBoard)
where

import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import Control.Monad (foldM)
import Data.Bits (testBit, (.&.), complement, popCount, bit)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (foldl', intersperse, intercalate, find, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Word (Word16)

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
