{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent (forkIO)
import Control.Monad (foldM, forM_)
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', intersperse, intercalate, (\\))
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

data Cell = Cell {-# UNPACK #-} !Int
                 ![Int]
                 {-# UNPACK #-} !Int
                 deriving (Eq)

data Board = Board { ixMap :: !(M.Map Int Cell),
                     ambCells :: !(S.Set Cell)
                   } deriving (Eq, Ord, Show)

instance Show Cell where
  show (Cell ix val _) = "<" ++ show ix ++ " " ++ show val ++ ">"

instance Ord Cell where
  (Cell i1 v1 vl1) `compare` (Cell i2 v2 vl2)
    | i1 == i2 && v1 == v2 = EQ
    | otherwise            = (vl1, i1) `compare`(vl2, i2)

emptyBoard :: Board
emptyBoard =
  Board (foldl' (\m c@(Cell i _ _) -> M.insert i c m) M.empty cells)
        (S.fromList cells)
  where cells = map (\i -> Cell i [1..9] 9) [0..80]

updateBoard :: Board -> Cell -> Board
updateBoard board@Board{..} cell@(Cell ix _ vl) = case M.lookup ix ixMap of
  Nothing      -> board
  Just oldCell | vl == 1   -> Board (M.insert ix cell ixMap)
                                    (S.delete oldCell ambCells)
               | otherwise -> Board (M.insert ix cell ixMap)
                                    (S.insert cell (S.delete oldCell ambCells))

constrainCell :: Cell -> Board -> Cell -> Maybe Board
constrainCell cell@(Cell _ val vl) board@Board{..} c@(Cell i pos pl) =
  case () of _
              | c == cell            -> return board
              | null pos' && vl == 1 -> Nothing
              | null pos'            -> return board
              | pl' == 1 && pl > 1   -> constrainBoard board (Cell i pos' pl')
              | otherwise            -> return $ updateBoard board (Cell i pos' pl')
  where
    pos' = pos \\ val
    pl' = length pos'

constrainCells :: Cell -> Board -> [Cell] -> Maybe Board
constrainCells cell = foldM (constrainCell cell)

constrainBoard :: Board -> Cell -> Maybe Board
constrainBoard board cell@(Cell ix _ _) =
  foldM (\board'' unitf -> constrainCells cell board'' (unitf board''))
        (updateBoard board cell) [row, column, box]
  where
    (rowIx, colIx) = ix `divMod` 9
    (rowIx', colIx') = ((rowIx `div` 3) * 3, (colIx `div` 3) * 3)

    cells board = map (fromJust . flip M.lookup (ixMap board))
    row board = cells board $ take 9 [rowIx * 9 ..]
    column board = cells board $ take 9 [colIx, colIx + 9 ..]
    box board =
      cells board [r * 9 + c | r <- [rowIx' .. rowIx' + 2], c <- [colIx' .. colIx' + 2]]

readBoard :: String -> Maybe Board
readBoard str =
  foldM constrainBoard emptyBoard
  . map (\(ix, n) -> Cell ix [digitToInt n] 1)
  . filter ((/= '.') . snd)
  . zip [0..] $ str

showBoard :: Board -> String
showBoard board =
  zipWith (\(Cell _ val vl) dot -> if vl == 1 then intToDigit (head val) else dot)
          (map snd . M.toList . ixMap $ board)
          (repeat '.')

printBoard :: Board -> IO ()
printBoard board =
  putStrLn
  . (\t -> line ++ "\n" ++ t ++ line ++ "\n")
  . unlines . intercalate [line] . chunksOf 3
  . map ((\r -> "| " ++ r ++ " |")
         . intercalate " | " . map (intersperse ' ') . chunksOf 3)
  . chunksOf 9
  . showBoard $ board
  where line = "+-------+-------+-------+"

solveSudoku :: Board -> Maybe Board
solveSudoku board
  | isSolved board          = Just board
  | S.null (ambCells board) = Nothing
  | null val                = solveSudoku (board { ambCells = cs })
  | otherwise               = let
      nextPos = Cell ix [head val] 1
      restPos = Cell ix (tail val) (vl - 1)
      boardR = updateBoard board restPos
      in case constrainBoard board nextPos of
           Nothing -> solveSudoku boardR
           Just board' | isSolved board' -> Just board'
                       | otherwise       -> case solveSudoku board' of
                           Just board''  -> Just board''
                           Nothing       -> solveSudoku boardR
  where
    ((Cell ix val vl), cs) = S.deleteFindMin (ambCells board)
    isSolved = all (\(Cell _ _ vl) -> vl == 1) . M.elems . ixMap

main :: IO ()
main = do
  lns <- fmap lines getContents
  forM_ lns $ \line -> forkIO $ do
    start <- getCPUTime
    let sudoku = readBoard line
    case sudoku of
      Nothing -> putStrLn ("Invalid input sudoku: " ++ line)
      Just board -> do
        let !res = solveSudoku board
        end <- getCPUTime
        let diff = fromIntegral (end - start) / (10 ^ 12)

        putStrLn (printf "%s -> %s [%0.3f sec]" line
                 (maybe "Unsolvable" showBoard res) (diff :: Double))