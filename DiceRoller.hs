{-
  A solution to rubyquiz 61 (http://rubyquiz.com/quiz61.html).

  The task for this Quiz is to write a dice roller. The program should take
  two arguments: a dice expression followed by the number of times to roll it
  (being optional, with a default of 1).

  The solution is done using Parsec for parsing the expression into an AST and
  then evaluating it recursively.

  Usage: bin/DiceRoller "(5d5-4)d(16/d4)+3" 10
         bin/DiceRoller 3d3

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module DiceRoller (RandomState, Expr(..), eval, expr, main) where

import Control.Applicative ((<$>), (<*), (*>), (<|>))
import Control.Monad (foldM, liftM2, liftM, when)
import Control.Monad.State (State, get, put, runState)
import System.Random (Random, StdGen, randomR, newStdGen)
import Text.Parsec (many1, digit, spaces, char, parse)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import System.Environment (getArgs)

-- Randomness setup for dice roll --

type RandomState = State StdGen

getRandomR :: Random a => (a, a) -> RandomState a
getRandomR limits = do
  gen <- get
  let (val, gen') = randomR limits gen
  put gen'
  return val

-- AST --

-- Expression AST types
data Expr = Lit Int       | -- An integer literal
            Add Expr Expr | -- Binary addition
            Sub Expr Expr | -- Binary subtraction
            Mul Expr Expr | -- Binary multiplication
            Div Expr Expr | -- Binary integer division
            Rol Expr      | -- Unary single dice roll
            MRol Expr Expr  -- Binary multiple dice rolls
            deriving (Show)

-- Recursively evaluates the AST to get its value
eval :: Expr -> RandomState Int
eval (Lit i)            = return i
eval (Add e1 e2)        = liftM2 (+) (eval e1) (eval e2)
eval (Sub e1 e2)        = liftM2 (-) (eval e1) (eval e2)
eval (Mul e1 e2)        = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2)        = liftM2 div (eval e1) (eval e2)

-- Evaluates sides and choose a random number between 1 and sides
eval (Rol sides)        = eval sides >>= \s -> getRandomR (1, s)

-- Evaluates dices and sides and accumulates over choosing random numbers between
-- 1 and sides, dice times
eval (MRol dices sides) = do
  d <- eval dices
  s <- eval sides
  foldM (\sum _ -> liftM (sum +) $ getRandomR (1, s)) 0 [1..d]

-- Parsers --

-- A parser that modifies the argument parser to accept whitespace after it
spaced = (<* spaces)

-- A parser to parse the integer literals
literal = (Lit . read) <$> spaced (many1 digit)

-- A parser to parse a unary operator followed by a factor
unaryOpFactor = spaced (char 'd') *> (Rol <$> factor)

-- A parse to parse a factor, where a factor is either a literal or
-- a factor preceded by an unary operator or an expression enclosed in brackets
factor  = spaced (char '(') *> spaced expr <* spaced (char ')')
          <|> unaryOpFactor
          <|> literal

-- Operators table in descending order of precedence
table = [[bop 'd' MRol AssocLeft],                       -- multiple rolls
         [bop '*' Mul AssocLeft, bop '/' Div AssocLeft], -- multiplication and division
         [bop '+' Add AssocLeft, bop '-' Sub AssocLeft]] -- addition and subtraction
  where bop c f = Infix (spaced (char c) *> return f)    -- binary operators

-- A parser to parse the full expression
expr = buildExpressionParser table factor

-- Main --

-- Reads the expression from program arguments, parses it and if successful,
-- evaluates the AST and displays the resultant values
main = do
  args <- getArgs
  when (null args) (error "Usage: DiceRoller <expr> [<times>]")

  let (str, times) = if length args == 1 then (head args, 1) else (args !! 0, read $ args !! 1)

  case parse expr "DiceRollParser" str of
    Left err -> putStrLn $ "Error while parsing: " ++ show err
    Right ast -> do
      g <- newStdGen
      foldM (\g' _ -> do
        let (val, g'') = runState (eval ast) g'
        putStr $ show val ++ " "
        return g'')
        g [1 .. times]
      return ()
