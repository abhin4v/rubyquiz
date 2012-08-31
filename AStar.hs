{-# LANGUAGE MultiParamTypeClasses #-}

module AStar where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromJust)

-- A* algorithm: Find a path from initial node to goal node using a heuristic function.
-- Returns Nothing if no path found. Else returns Just (path cost, path).
astar :: (Ord a, Ord b, Num b) => a -> a -> (a -> [(a, b)]) -> (a -> a -> b) -> Maybe (b, [a])
astar initNode goalNode nextNode hueristic =
  astar' (PQ.singleton (hueristic initNode goalNode) (initNode, 0))
         S.empty (M.singleton initNode 0) M.empty
  where
    -- pq: open set, seen: closed set, tracks: tracks of states
    astar' pq seen gscore tracks
      -- If open set is empty then search has failed. Return Nothing
      | PQ.null pq = Nothing
      -- If goal node reached then construct the path from the tracks and node
      | node == goalNode = Just (gcost, findPath tracks node)
      -- If node has already been seen then discard it and continue
      | S.member node seen = astar' pq' seen gscore tracks
      -- Else expand the node and continue
      | otherwise = astar' pq'' seen' gscore' tracks'
      where
        -- Find the node with min f-cost
        (node, gcost) = snd . PQ.findMin $ pq

        -- Delete the node from open set
        pq' = PQ.deleteMin pq

        -- Add the node to the closed set
        seen' =  S.insert node seen

        -- Find the successors (with their g and h costs) of the node
        -- which have not been seen yet
        successors =
          filter (\(s, g, _) ->
                    not (S.member s seen') &&
                      (not (s `M.member` gscore)
                        || g < (fromJust . M.lookup s $ gscore)))
         $ successorsAndCosts node gcost

        -- Insert the successors in the open set
        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors

        gscore' = foldl' (\m (s, g, _) -> M.insert s g m) gscore successors

        -- Insert the tracks of the successors
        tracks' = foldl' (\m (s, _, _) -> M.insert s node m) tracks successors

    -- Finds the successors of a given node and their costs
    successorsAndCosts node gcost =
      map (\(s, g) -> (s, gcost + g, hueristic s goalNode)) . nextNode $ node

    -- Constructs the path from the tracks and last node
    findPath tracks node =
      if M.member node tracks
      then findPath tracks (fromJust . M.lookup node $ tracks) ++ [node]
      else [node]