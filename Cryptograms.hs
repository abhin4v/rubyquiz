{-|
  A solution to rubyquiz 13 (<http://rubyquiz.com/quiz13.html>).

  Decrypts a cryptogram (a substitution cypher).

  The decryption mapping is created by finding the words from the dictionary which have
  the same form as the given token to translates, using the words to create partial
  mappings and then merging these partial mappings to get more complete mappings.

  Similarly, the partial mappings for all tokens are merged together to get the
  complete mapping. The mappings are scored by how many tokens they are able to
  translated successfully into dictionary words.

  Usage:

  > ./Cryptograms <dictionary_file> <encrypted_file> <num_max_mappings>

  Copyright 2012 Abhinav Sarkar \<abhinav\@abhinavsarkar.net\>
-}

{-# LANGUAGE BangPatterns #-}

module Cryptograms (Mapping, Token, Dict, readDict, translateToken,
                    scoreMapping, findBestMappings, showMapping, main)
where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM)
import Data.Char (toLower, isAlpha)
import Data.List (foldl', find, sortBy, nub)
import Data.Maybe (isJust, fromJust, mapMaybe, catMaybes, fromMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)
-- import Debug.Trace (trace)

trace :: String -> a -> a
trace _ x = x

-- | A mapping between two alphabets.
type Mapping = M.Map Char Char

-- | An encrypted token.
type Token = String

-- | The dictionary of words.
newtype Dict = Dict (M.Map Int (S.Set String))

-- | Reads the dictionary from the given file. Must contain one word per line.
readDict :: FilePath -> IO Dict
readDict filePath = do
  !dictWords <- fmap (filter (all isAlpha) . map (map toLower) . lines)
                  $ readFile filePath
  return . Dict $
    foldl' (\dict w -> M.insertWith S.union (length w) (S.singleton w) dict)
          M.empty dictWords

-- | Translates the token using the given mapping.
-- Returns @Nothing@ if unable to translate.
translateToken :: Mapping -> Token -> Maybe String
translateToken mapping = fmap reverse
  . foldM (\acc char -> M.lookup char mapping >>= Just . (:acc)) ""

-- Translates all tokens using the given mapping.
-- Translates the token to '---' if unable to translate.
translateTokens :: Mapping -> [Token] -> [String]
translateTokens mapping =
  map (\token ->
        fromMaybe (replicate (length token ) '-') . translateToken mapping $ token)

-- Checks if the given word is in the dictionary.
inDict :: Dict -> String -> Bool
inDict (Dict dict) word =
  case M.lookup (length word) dict of
    Nothing -> False
    Just ws -> word `S.member` ws

-- | Scores a mapping by counting the number of translated tokens that are
-- in the dictionary.
scoreMapping :: Dict        -- ^ The dictionary
                -> Mapping  -- ^ The mapping to score
                -> [Token]  -- ^ The tokens to translate
                -> Int      -- ^ The score of the mapping
scoreMapping dict mapping =
  length . filter (inDict dict) . mapMaybe (translateToken mapping)

-- Scores multiple mappings and returns an assoc list sorted by descending score.
scoreMappings :: Dict -> [Token] -> [Mapping] -> [(Mapping, Int)]
scoreMappings dict tokens =
  reverse . sortBy (comparing snd)
  . map (\mapping -> (mapping, scoreMapping dict mapping tokens))

-- | Finds n best mappings which have best scores for the given tokens.
findBestMappings :: Dict          -- ^ The dictionary
                    -> Int        -- ^ Maximum number of mappings to consider
                    -> [Token]    -- ^ The tokens to translate
                    -> [Mapping]  -- ^ The mappings with best scores
findBestMappings dict num tokens = let
  mappings = scoreMappings dict tokens
             . S.toList
             . foldl' (\mappings -> -- find the best num mappings
                        S.fromList . take num
                        . map fst . scoreMappings dict tokens . S.toList
                        . findMappingsForToken dict mappings)
                      S.empty
             . nub . reverse . sortBy (comparing (\x -> (length x, x)))
             $ tokens
  maxScore = if not (null mappings) then snd . head $ mappings else 0
  in map fst . takeWhile ((== maxScore) . snd) $ mappings

-- Finds the merged mappings for a token
findMappingsForToken :: Dict -> S.Set Mapping -> Token  -> S.Set Mapping
findMappingsForToken dict mappings token =
  case find (inDict dict) . mapMaybe (flip translateToken token)
       . reverse . sortBy (comparing M.size)
       . S.toList $ mappings of
    -- the token is already translatable. return current mappings.
    Just dtoken -> trace (printf "Translated %s -> %s" token dtoken) mappings

    -- the token is not translatable yet. return current mappings merged
    -- with the mappings for the token.
    Nothing -> mergeMappingLists mappings (createMappingsForToken dict token)

-- Merges mapping lists. discards conflicting mappings while merging.
mergeMappingLists :: S.Set Mapping -> S.Set Mapping -> S.Set Mapping
mergeMappingLists mappings1 mappings2
  | mappings1 == S.empty = mappings2
  | mappings2 == S.empty = mappings1
  | otherwise =
      trace (printf "Merging %s x %s mappings" (show . S.size $ mappings1) (show . S.size $ mappings2)) $
        let
          merged = -- union current mappings and their merged result mappings
            S.unions [mappings1, mappings2,
              S.fromList . catMaybes $
                [mergeMappings m1 m2 | m1 <- S.toList mappings1, m2 <- S.toList mappings2]]
        in trace (printf "Merged to %s mappings" (show $ S.size merged)) merged

-- Merges two mappings. returns Nothing if mappings conflict.
mergeMappings :: Mapping -> Mapping -> Maybe Mapping
mergeMappings mapping1 mapping2 =
  foldM
    (\acc (k, v) ->
      if M.member k acc
        then if (fromJust . M.lookup k $ acc) == v then Just acc else Nothing
        else Just $ M.insert k v acc)
    mapping1 $ M.toList mapping2

-- Creates mappings for a token by finding words of same form from the dictionary.
createMappingsForToken :: Dict -> Token -> S.Set Mapping
createMappingsForToken (Dict dict) token =
  case M.lookup (length token) dict of
    Nothing -> S.empty
    Just words -> let
      tokenF = tokenForm token
      matches = S.fromList . map (getMapping token)
                . filter ((== tokenF) . tokenForm) . S.toList $ words
      in trace (printf "%s -> %s matches" token (show . S.size $ matches)) matches

-- Returns form of a token. for example, the form of "abc" is [1,2,3]
-- while the form of "aba" is [1,2,1].
tokenForm :: Token -> [Int]
tokenForm token = let
  (_, form, _) =
    foldl' (\(formMap, form, lf) char ->
             case M.lookup char formMap of
               Nothing -> (M.insert char (lf + 1) formMap, (lf + 1) : form, lf + 1)
               Just f -> (formMap, f : form, lf))
           (M.empty, [], 0) token
  in reverse form

-- Creates the mapping between two strings of same length.
getMapping :: String -> String -> Mapping
getMapping t1 t2 = M.fromList $ zip t1 t2

-- | Returns text representation of a mapping.
showMapping :: Mapping -> String
showMapping mapping =
  map snd . sortBy (comparing fst) . M.toList
  . foldl' (\acc c -> M.insertWith (\_ l -> l) c '.' acc) mapping $ ['a'..'z']

main :: IO()
main = do
  (dictFile : cryptTextFile : num : _) <- getArgs
  -- read the dictionary
  !dict <- readDict dictFile
  -- read the encrypted tokens
  !tokens <- fmap (map (map toLower) . lines) $ readFile cryptTextFile

  let mappings = findBestMappings dict (read num) tokens

  if not (null mappings)
    then do
      putStrLn $ printf "%s best mappings found with score %s/%s"
        (show $ length mappings)
        (show $ scoreMapping dict (head mappings) tokens)
        (show $ length tokens)
      putStrLn . unlines $
        map (\mapping -> printf "%s -> %s"
              (showMapping mapping)
              (unwords . translateTokens mapping $ tokens))
            mappings
    else
      putStrLn "No mappings found"