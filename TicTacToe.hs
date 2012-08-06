{-# LANGUAGE BangPatterns #-}

module TicTacToe where

import Data.List (sort, nub, maximumBy)
import Data.List.Split (chunk)
import Data.Ord (comparing)
import System.Random (Random, StdGen, randomR, newStdGen, split)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import Control.Monad.State (State, get, put, runState, evalState)
import qualified Data.Map as M

-- Randomness setup

type RandomState = State StdGen

getRandomR :: Random a => (a, a) -> RandomState a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

randomChoose :: [a] -> RandomState a
randomChoose list = do
  i <- getRandomR (0, length list - 1)
  return $ list !! i

toss :: RandomState Bool
toss = randomChoose [True, False]

-- Board setup

data Move = Nought | Cross deriving (Eq, Ord)

data CellState = Filled Move | Empty deriving (Eq, Ord)

data Cell = Cell {cellPos :: Int, cellState :: CellState} deriving (Eq, Ord)

type Board = [Cell]

type Run = [Board]

data Result = Win | Loss | Draw | Unfinished deriving (Eq, Show)

instance Show Move where
  show Nought = "O"
  show Cross  = "X"

instance Show CellState where
  show (Filled move) = show move
  show Empty = "~"

instance Show Cell where
  show c = show $ cellState c

otherMove :: Move -> Move
otherMove Nought = Cross
otherMove Cross = Nought

otherResult :: Result -> Result
otherResult Draw = Draw
otherResult Loss = Win
otherResult Win = Loss

emptyBoard :: Board
emptyBoard = map (flip Cell Empty) [0..8]

printBoard :: Board -> IO ()
printBoard board = putStrLn "" >> (mapM_ print . chunk 3 $ board)

makeMove :: Int -> Move -> Board -> Board
makeMove pos move board =
  let (l, r) = splitAt pos board
  in l ++ [Cell pos (Filled move)] ++ tail r

diags :: Board -> [[Cell]]
diags board =
  [[board !! 0, board !! 4, board !! 8],
   [board !! 2, board !! 4, board !! 6]]

nextBoards :: Move -> Board -> [(Int, Board)]
nextBoards move board =
  map ((\p -> (p, makeMove p move board)) . cellPos)
  $ filter (\c -> cellState c == Empty) board

isWin :: Move -> Board -> Bool
isWin move board =
  or [any isStrike $ chunk 3 $ map cellState board,
      any isStrike $ chunk 3 $ map cellState $ rotateBoard board,
      any isStrike $ map (map cellState) $ diags board]
  where
    isStrike = (== replicate 3 (Filled move))

result :: Move -> Board -> Result
result move board
  | isWin move board                 = Win
  | isWin (otherMove move) board     = Loss
  | Empty `elem` map cellState board = Unfinished
  | otherwise                        = Draw

translateBoard :: [Int] -> Board -> Board
translateBoard idxs board =
  map (\(i, ri) -> Cell i $ cellState $ board !! ri) $ zip [0..8] idxs

rotateBoard, xMirrorBoard, yMirrorBoard :: Board -> Board
rotateBoard  = translateBoard [6,3,0,7,4,1,8,5,2]
xMirrorBoard = translateBoard [2,1,0,5,4,3,8,7,6]
yMirrorBoard = translateBoard [6,7,8,3,4,5,0,1,2]

rotateBoardN :: Board -> Int -> Board
rotateBoardN board n = foldl (\b _ -> rotateBoard b) board [1..n]

-- Player setup

class Player a where
  playerMove :: a -> Move
  play :: a -> Board -> (a, Board)
  improvePlayer :: a -> Result -> Run -> a

-- play a match between two players
playMatch :: (Player p1, Player p2) => p1 -> p2 -> (Result, Run, p1, p2)
playMatch player1 player2 = playMatch_ player1 player2 emptyBoard

playMatch_ :: (Player p1, Player p2) => p1 -> p2 -> Board -> (Result, Run, p1, p2)
playMatch_ player1 player2 board =
  case result (playerMove player1) board of
    Unfinished -> let
      (player1', board') = play player1 board
      in case result (playerMove player1) board' of
        Unfinished -> let
          (res', run, player2', player1'') = playMatch_ player2 player1' board'
          in (otherResult res', board' : run, player1'', player2')
        res -> (res, [], player1', player2)
    res -> (res, [], player1, player2)

-- play multiple matches between two players
playMatches :: (Player p1, Player p2) => Int -> p1 -> p2 -> ([(Result, Run)],p1, p2)
playMatches times player1 player2 =
  foldl (\(matches, p1, p2) _ ->
    let
      (res, run, p1', p2') = playMatch p1 p2
      p1'' = improvePlayer p1' res run
      p2'' = improvePlayer p2' (otherResult res) run
    in  ((res, run) : matches, p1'', p2''))
  ([], player1, player2) [1..times]

-- RandomPlayer setup

-- play randomly. choose a random move
randomPlay :: Move -> Board -> RandomState Board
randomPlay move board = randomChoose (map snd $ nextBoards move board)

data RandomPlayer = RandomPlayer Move StdGen deriving (Show)

instance Player RandomPlayer where
  playerMove (RandomPlayer move _) = move
  play (RandomPlayer move gen) board =
    let
      (board', gen') = runState (randomPlay move board) gen
    in (RandomPlayer move gen', board')
  improvePlayer player _ _ = player

-- LearningPlayer setup

type Memory = M.Map Board (Int, Int, Int)

-- boards equivalent to this board
eqvBoards :: Board -> [Board]
eqvBoards board = nub . sort $
  board : map (rotateBoardN board) [1..3] ++ [xMirrorBoard board, yMirrorBoard board]

data LearningPlayer = LearningPlayer Move Memory StdGen deriving (Show)

-- play using the strategy learned till now
learningPlay :: LearningPlayer -> Board -> (LearningPlayer, Board)
learningPlay (LearningPlayer move mem gen) board = let
  next = map snd $ nextBoards move board
  in case filter (isWin move) next of
    (winBoard:_) -> (LearningPlayer move mem gen, winBoard)
    [] -> let
      otherNext = nextBoards (otherMove move) board
      in case filter (isWin (otherMove move) . snd) otherNext of
        ((pos,_):_) -> (LearningPlayer move mem gen, makeMove pos move board)
        [] -> let
          scores = map (\b -> (b, boardScore b mem)) $ next
          (board', (w, _, d)) = maximumBy (comparing (calcScore . snd)) scores
          in if w /= 0
             then (LearningPlayer move mem gen, board')
             else let
               ((rBoard, _), gen') = runState (randomChoose scores) gen
             in (LearningPlayer move mem gen', rBoard)
  where
    boardScore board' mem =
      foldl (\score b' -> sumScores score $ M.findWithDefault (0, 0, 0) b' mem)
            (0, 0, 0) (eqvBoards board')
    sumScores (w, l, d) (w', l', d') = (w + w', l + l', d + d')

calcScore :: (Int, Int, Int) -> Double
calcScore (w, l, d) = fromIntegral w + fromIntegral d * 0.5 - fromIntegral l

-- learn strategy from the run
learnFromRun :: Result -> Run -> Memory -> Memory
learnFromRun res run mem = let
  score = incrementScore res (0, 0, 0)
  mem' = foldl (\m b -> M.insertWith (\_ -> incrementScore res) b score m)
               mem run
  in mem'
  where
    incrementScore res (w, l, d) =
      case res of
        Win  -> (w + 1, l, d)
        Loss -> (w, l + 1, d)
        Draw -> (w, l, d + 1)

instance Player LearningPlayer where
  playerMove (LearningPlayer move _ _) = move
  play = learningPlay
  improvePlayer (LearningPlayer move mem gen) res run =
    LearningPlayer move (learnFromRun res run mem) gen

-- play two LearningPlayers against each other to learn strategy
learnedPlayer :: Move -> StdGen -> LearningPlayer
learnedPlayer move gen = let
  (gen1, gen2) = split gen
  p1 = LearningPlayer move M.empty gen1
  p2 = LearningPlayer (otherMove move) M.empty gen2
  (_, p1', p2') = playMatches 1000 p1 p2
  in p1'

-- Play against human

-- play a player against a human. human enters moves from prompt.
playHuman :: Player p => p -> Board -> IO ()
playHuman player board = do
  printBoard board
  case result (playerMove player) board of
    Unfinished -> do
      putStr "Move? "
      pos <- fmap (decr . read) getLine
      if pos < 0 || pos > 8
      then do
        putStrLn "Invalid Move"
        playHuman player board
      else
        case cellState (board !! pos) of
          Filled _ -> do
            putStrLn "Invalid Move"
            playHuman player board
          Empty -> let
            board' = makeMove pos Nought board
            in case result (playerMove player) board' of
              Unfinished -> let
                (player', board'') = play player board'
                in playHuman player' board''
              res -> do
                printBoard board'
                putStrLn ("Your " ++ show (otherResult res))
    res -> putStrLn ("Your " ++ show (otherResult res))
  where decr x = x - 1

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  gen <- newStdGen
  putStrLn "Learning ..."
  let !player = learnedPlayer Cross gen
  putStrLn "Learned"
  putStrLn "Tossing for first move"
  let t = evalState toss gen
  if t
  then do
    putStrLn "You win toss"
    playHuman player emptyBoard
  else do
    putStrLn "You lose toss"
    let (player', board) = play player emptyBoard
    playHuman player' board
