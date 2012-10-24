{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Main (main) where

import qualified Data.Set as S
import qualified Data.HashMap.Strict as M
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (foldM, forM_, forM, (>=>))
import Data.Bits (testBit, (.&.), complement, popCount, bit)
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', intersperse, intercalate, find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

data Cell = Cell {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Word16
                 {-# UNPACK #-} !Int

data Board = Board { ixMap :: !(M.HashMap Int Cell),
                     ambCells :: !(S.Set Cell)
                   } deriving (Eq, Show)

instance Eq Cell where
  {-# INLINE (==) #-}
  (Cell i1 v1 _) == (Cell i2 v2 _) = i1 == i2 && v1 == v2

instance Show Cell where
  show (Cell ix val _) = "<" ++ show ix ++ " " ++ show val ++ ">"

instance Ord Cell where
  (Cell i1 v1 vl1) `compare` (Cell i2 v2 vl2) =
    if i1 == i2 && v1 == v2
    then EQ
    else (vl1, i1) `compare`(vl2, i2)

firstSol :: Word16 -> Int
firstSol val = fromJust . find (testBit val) $ [1..9]

emptyBoard :: Board
emptyBoard =
  Board (foldl' (\m c@(Cell i _ _) -> M.insert i c m) M.empty cells)
        (S.fromList cells)
  where cells = map (\i -> Cell i 1022 9) [0..80]

updateBoard :: Board -> Cell -> Board
updateBoard board@Board{..} cell@(Cell ix _ vl) = case M.lookup ix ixMap of
  Nothing      -> board
  Just oldCell | oldCell == cell -> board
               | vl == 1         -> Board (M.insert ix cell ixMap)
                                          (S.delete oldCell ambCells)
               | otherwise       -> Board (M.insert ix cell ixMap)
                                          (S.insert cell (S.delete oldCell ambCells))

constrainCell :: Cell -> Board -> Cell -> Maybe Board
constrainCell cell@(Cell _ val vl) board@Board{..} c@(Cell i pos pl) =
  case () of _
              | c == cell            -> return board
              | pos' == 0 && vl == 1 -> Nothing
              | pos' == 0            -> return board
              | pl' == 1 && pl > 1   -> constrainBoard board (Cell i pos' pl')
              | otherwise            -> return $ updateBoard board (Cell i pos' pl')
  where
    pos' = pos .&. complement val
    pl' = popCount pos'

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
  . map (\(ix, n) -> Cell ix (bit $ digitToInt n) 1)
  . filter ((/= '.') . snd)
  . zip [0..] $ str

showBoard :: Board -> String
showBoard board =
  zipWith (\(Cell _ val vl) dot ->
              if vl == 1 then intToDigit . firstSol $ val else dot)
          (M.elems . ixMap $ board)
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
  | val == 0                = solveSudoku $ board { ambCells = cs }
  | otherwise               = let
      fs = bit . firstSol $ val
      nextPos = Cell ix fs 1
      restPos = Cell ix (val .&. complement fs) (vl - 1)
      boardR = updateBoard board restPos
      in case constrainBoard board nextPos of
           Nothing -> solveSudoku boardR
           Just board' | isSolved board' -> Just board'
                       | otherwise       -> case solveSudoku board' of
                           Just board''  -> Just board''
                           Nothing       -> solveSudoku boardR
  where
    (Cell ix val vl, cs) = S.deleteFindMin (ambCells board)
    isSolved = all (\(Cell _ _ vl) -> vl == 1) . M.elems . ixMap

main :: IO ()
main = do
  chunks <- fmap (chunksOf 10 . lines) getContents
  threads <- forM chunks $ \chunk -> do
    done <- newEmptyMVar
    forkIO $ do
      sols <- forM chunk $ \line -> do
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
      putMVar done sols
    return done

  forM_ threads $ takeMVar >=> mapM_ putStrLn
