{-
  A solution to rubyquiz 20 (http://rubyquiz.com/quiz20.html).

  Many companies like to list their phone numbers using the letters printed on
  most telephones. This makes the number easier to remember for customers.
  A famous example being 1-800-PICK-UPS.

  This quiz is to write a program that will show a user possible
  matches for a list of provided phone numbers.

  Usage: cat phonenumbers | ./PhoneNumberWords dictionary_file

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (isAscii, isAlpha, toUpper, isDigit)
import Data.List (foldl', sort, intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

type Dict = M.Map String (S.Set String)

-- reads the dictionary from the given file. must contain one word per line.
readDict :: FilePath -> IO Dict
readDict filePath = do
  !dictWords <- fmap (filter (all isAlpha) . filter (all isAscii)
                      . filter ((> 2) . length)
                      . map (map toUpper) . lines)
                  $ readFile filePath
  return $
    foldl' (\dict w -> M.insertWith S.union (translate w) (S.singleton w) dict)
          M.empty dictWords

-- find all possible splits of a list
splits :: [a] -> [[[a]]]
splits [] = []
splits [x] = [[[x]]]
splits (x:xs) =
  concatMap (\sp -> [[x] : sp, (x : head sp) : tail sp]) $ splits xs

sliding :: Int -> Int -> [a] -> [[a]]
sliding _ _ [] = []
sliding size step xs
  | length xs >= size = take size xs : sliding size step (drop step xs)
  | otherwise = []

-- translate a word to a phone number
translate :: String -> String
translate = map translateChar
  where
    translateChar c
      | c `S.member` S.fromList "ABC"  = '2'
      | c `S.member` S.fromList "DEF"  = '3'
      | c `S.member` S.fromList "GHI"  = '4'
      | c `S.member` S.fromList "JKL"  = '5'
      | c `S.member` S.fromList "MNO"  = '6'
      | c `S.member` S.fromList "PQRS" = '7'
      | c `S.member` S.fromList "TUV"  = '8'
      | c `S.member` S.fromList "WXYZ" = '9'

-- find all the words for a split for a phone number
wordsForSplit dict =
  map (\k -> S.toList . fromMaybe (S.singleton k) . M.lookup k $ dict)

-- find all phone number words for a phone number
phoneNumberWords dict =
  filter isValid . sort
  . concatMap (map (drop 1)
               . foldl (\acc ws -> [a ++ "-" ++ w | a <- acc, w <- ws]) [[]]
               . wordsForSplit dict)
  . splits
  where
    isValid = not . any (all isDigit) . sliding 2 1 . filter (/= '-')

main = do
  (dictFileName : _) <- getArgs
  !dict <- readDict dictFileName
  interact (unlines . map (intercalate ", " . phoneNumberWords dict) . lines)
