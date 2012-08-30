{-
  A solution to rubyquiz 25 (http://rubyquiz.com/quiz25.html).

  When the integers 1 to 10_000_000_000 are written in the English language,
  then sorted as strings, which odd number appears first in the list?

  Usage: ./EnglishNumerals basis-file max_num

  Example basis file for English numerals:

  > 1000000000000000000, quintillion
  > 1000000000000000, quadrillion
  > 1000000000000, trillion
  > 1000000000, billion
  > 1000000, million
  > 1000, thousand
  > 100, hundred
  > 90, ninety
  > 80, eighty
  > 70, seventy
  > 60, sixty
  > 50, fifty
  > 40, forty
  > 30, thirty
  > 20, twenty
  > 19, nineteen
  > 18, eighteen
  > 17, seventeen
  > 16, sixteen
  > 15, fifteen
  > 14, fourteen
  > 13, thirteen
  > 12, twelve
  > 11, eleven
  > 10, ten
  > 9, nine
  > 8, eight
  > 7, seven
  > 6, six
  > 5, five
  > 4, four
  > 3, three
  > 2, two
  > 1, one

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

module EnglishNumerals where

import qualified Data.Sequence as Seq
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

type Basis = [(Integer, String)]

isPowerOfTen :: Integer -> Bool
isPowerOfTen = (== "10") . nub . show

-- reads the basis of the numeral system
readBasis :: FilePath -> IO Basis
readBasis =
  fmap (map (\line -> let (n:en:_) = splitOn "," line in (read n, en)) . lines) . readFile

cache basis = fmap (toEnglishNumerals basis) $ Seq.fromList [1..999]

toEnglishNumeralsMemo :: Basis -> Integer -> String
toEnglishNumeralsMemo basis n =
  if n < 1000
    then cache basis `Seq.index` (fromIntegral n -1)
    else toEnglishNumerals basis n

-- converts a number to its numeral representation in the given basis
toEnglishNumerals :: Basis -> Integer -> String
toEnglishNumerals basis n =
  unwords . words . go n (dropWhile ((> n) . fst) basis) $ ""
  where
    go 0 _ eng = eng
    go n [] eng = eng
    go n basis eng =
      case lookup n basis of
        Just nu -> eng ++ (if b > 90 then "one" else  "") ++ " " ++ nu
        Nothing ->
          case n `divMod` b of
            (0, r) -> go r (tail basis) eng
            (q, 0) -> eng ++ toEnglishNumeralsMemo basis q ++ " " ++ bn
            (1, r) -> eng ++ (if b > 90 then "one" else  "") ++ " " ++ bn
                      ++ " " ++ toEnglishNumeralsMemo basis r
            (q, r) -> eng ++ " " ++ toEnglishNumeralsMemo basis q ++ " " ++ bn
                      ++ " " ++ toEnglishNumeralsMemo basis r
        where (b, bn) = head basis

-- given a basis and a range of numbers specified by start, end and step,
-- finds the number in this range and its representation which is minimum
-- when the representations are sorted lexicographically
minEnglish :: Basis -> Integer -> Integer -> Integer -> (String, Integer)
minEnglish basis start end step =
  maximumBy (flip $ comparing fst)
  . map (\x -> (toEnglishNumerals basis x, x)) $ [start, start + step .. end]

-- finds the first odd number and its representation between 1 and n which is
-- minimum by the lexicographically sorted representations
firstOddByEnglishNumeral :: Basis -> Integer -> (String, Integer)
firstOddByEnglishNumeral basis n =
  (\(eng, en) -> (unwords . words $ eng, en))
  $ foldl
      (\(eng, en) (start, end, step) ->
        if n < start
        then (eng, en)
        else let
          (eng', en') = if n > fromMaybe n end
                          then minEnglish basis start (fromMaybe n end) step
                          else minEnglish basis start n step
          in
            if eng == ""
              then (eng' ++ " " ++ eng, en')
              else if eng' ++ " " ++ eng < eng && en + en' <= n
                   then (eng' ++ " " ++ eng, en + en')
                    else (eng, en))
      ("", 0) megas
  where
    megas =
      map (\(s,e) -> if s == 1 then (s,e,2) else (s,e,s))
      . zip cs
      $ map (\x -> Just $ x-1) (tail cs) ++ repeat Nothing
      where
        cs = 1 : (reverse . filter (>= 100) . filter isPowerOfTen . map fst $ basis)

main = do
  (basisFile : n : _) <- getArgs
  basis <- readBasis basisFile

  putStrLn . (\(ne, n) -> show n ++ " (" ++ ne ++ ")")
    . firstOddByEnglishNumeral basis . read $ n

