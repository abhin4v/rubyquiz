{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM, forM_)
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', intersperse, (\\), sortBy, groupBy)
import Data.List.Split (splitEvery)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

countingSortBy f lo hi =
  concatMap snd
  . A.assocs . A.accumArray (\ e a -> a : e) [] (lo, hi) . map (\i -> (f i, i))

data Cell = Cell !Int ![Int] deriving (Eq, Ord)
type Board = M.Map Int Cell

instance Show Cell where
  show cell@(Cell ix val) = "<" ++ show ix ++ " " ++ show val ++ ">"

emptyBoard =
  foldl' (\m i -> M.insert i (Cell i [1..9]) m) M.empty [0..80]

constrainCell cell@(Cell ix val) board c@(Cell i pos) =
  case () of _
              | c == cell                          -> return board
              | null pos' && length val == 1       -> Nothing
              | null pos'                          -> return board
              | length pos' == 1 && length pos > 1 -> constrainBoard board (Cell i pos')
              | otherwise                          -> return $ M.insert i (Cell i pos') board
  where pos' = pos \\ val

constrainCells :: Cell -> S.Set Cell -> Board -> [Cell] -> Maybe Board
constrainCells cell@(Cell ix val) seen board unit = do
  board' <- foldM (constrainCell cell) board unit
  let unit' = map (\(Cell ix _) -> fromJust $ M.lookup ix board') $ unit

  foldM (\board'' cell' ->
            constrainCells cell' (S.insert cell' seen) board'' unit')
        board' (dups unit')
  where
    dups = filter (not . flip S.member seen) . map head
           . filter (\xs@(Cell _ v : _) -> length xs == length v)
           . groupBy (\(Cell _ v1) (Cell _ v2) -> v1 == v2)
           . sortBy (comparing (\(Cell _ val) -> val))
           . filter (\(Cell _ v) -> length v > 1 && length v < 4)

constrainBoard :: Board -> Cell -> Maybe Board
constrainBoard board cell@(Cell ix _) =
  foldM (\board'' unitf ->
            constrainCells cell (S.singleton cell) board'' (unitf cell board''))
        (M.insert ix cell board) [row, column, box]
  where
    (rowIx, colIx) = ix `divMod` 9
    (rowIx', colIx') = ((rowIx `div` 3) * 3, (colIx `div` 3) * 3)

    cells board = map (fromJust . flip M.lookup board)
    row (Cell ix _) board = cells board $ take 9 [rowIx * 9 ..]
    column (Cell ix _) board = cells board $ take 9 [colIx, colIx + 9 ..]
    box (Cell ix _) board =
      cells board [r * 9 + c | r <- [rowIx' .. rowIx' + 2], c <- [colIx' .. colIx' + 2]]

readBoard :: String -> Maybe Board
readBoard str =
  foldM constrainBoard emptyBoard
  . map (\(ix, n) -> Cell ix [digitToInt n])
  . filter ((/= '.') . snd)
  . zip [0..] $ str

showBoard :: Board -> String
showBoard board =
  zipWith (\(Cell _ val) dot ->
              if length val == 1 then intToDigit (head val) else dot)
          (map snd . M.toList $ board)
          (repeat '.')

printBoard :: Board -> IO ()
printBoard board =
  putStrLn
  . (\t -> line ++ "\n" ++ t ++ line ++ "\n")
  . unlines . concat . intersperse [line] . splitEvery 3
  . map ((\r -> "| " ++ r ++ " |") . concat
         . intersperse " | " . map (intersperse ' ') . splitEvery 3)
  . splitEvery 9
  . showBoard $ board
  where line = "+-------+-------+-------+"

solveSudoku :: Board -> Maybe Board
solveSudoku = fst . flip solve S.empty
  where
    solve board invalid = go (cells board) board invalid

    cells board =
    -- countingSortBy (\(Cell _ val) -> length val) 2 9
      sortBy (comparing (\(Cell _ val) -> length val))
      . filter (\(Cell _ val) -> length val /= 1)
      . map snd . M.toList $ board

    isSolved = all (\(Cell _ val) -> length val == 1) . M.elems

    go [] board invalid = (Nothing, S.insert board invalid)
    go (Cell ix val : cs) board invalid
      | S.member board invalid = (Nothing, invalid)
      | null val               = go cs board (S.insert board invalid)
      | otherwise              = let
          nextPos = Cell ix [head val]
          restPos = Cell ix (tail val)
          board' = M.insert ix nextPos board
          in case constrainBoard board nextPos of
               Nothing -> go (restPos : cs) board (S.insert board' invalid)
               Just board'' | isSolved board'' -> (Just board'', invalid)
                            | otherwise        -> let
                                (mBoard', invalid') = solve board'' invalid
                                in case mBoard' of
                                     Just board''' -> (Just board''', invalid')
                                     Nothing       ->
                                       go (restPos : cs) board (S.insert board'' invalid')

main = do
  lns <- fmap lines getContents
  forM_ lns $ \line -> do
    start <- getCPUTime
    let sudoku = readBoard line
    case sudoku of
      Nothing -> putStrLn ("Invalid input sudoku: " ++ line)
      Just board -> do
        let !res = solveSudoku board
        end <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10^12)

        putStrLn (printf "%s -> %s [%0.3f sec]" line (maybe "Unsolvable" showBoard res) (diff :: Double))

        --putStrLn (printf "Time taken: %0.3f sec" (diff :: Double))
        --printBoard board

        --case res of
        --  Nothing -> putStrLn "Unsolvable"
        --  Just board' -> printBoard board'