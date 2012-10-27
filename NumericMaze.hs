{-
 A Solution to rubyquiz 60 (http://rubyquiz.com/quiz60.html)

 You have a starting point and a target. You have a set of three operations:
  double
  halve (Odd numbers cannot be halved)
  add_two
 Problem: Move from the starting point to the target, minimizing the number of operations.

 This solution finds the shortest path using A* search with cost of each operation
 as one and the heuristic as the absolute of log of the ratio of the target
 and start numbers.

 Usage: bin/NumericMaze 2 9

 Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

module NumericMaze (solve, main) where

import AStar
import Control.Monad (when)
import System.Environment (getArgs)

data Op = Double | Halve | AddTwo

execute :: Integral a => a -> Op -> a
execute n Double = 2 * n
execute n Halve  = let (q, r) = n `divMod` 2 in if r == 0 then q else n
execute n AddTwo = n + 2

solve start end =
  astar start end
    (\n -> zip (map (execute n) [Double, Halve, AddTwo]) (repeat 1))
    (\a b -> abs $ logBase 2 (fromIntegral b / fromIntegral a))

main = do
  (start : end : _) <- fmap (map read) getArgs

  when (start <= 0 || end <= 0) (error "Error: The numbers must be positive")

  case solve start end of
    Nothing -> putStrLn "No solution"
    Just (_, solution) -> putStrLn . unwords . map show $ solution