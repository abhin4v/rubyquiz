{-
  A solution to rubyquiz 30 (http://rubyquiz.com/quiz30.html).

  Copyright 2012 Abhinav Sarkar <abhinav@abhinavsarkar.net>
-}

{-# LANGUAGE Arrows, NoMonomorphismRestriction, RecordWildCards #-}

module BarrelOfMonkeys
  (Song(..), SongLibrary, Playlist(..), getSongsFromXml,
   shortestPlaylist, longestPlaylist, shortestTimePlaylist, longestTimePlaylist,
   timedPlaylist, main)
where

import qualified Data.Map as M
import qualified Data.FingerTree.PSQueue as Q
import qualified Data.Set as S
import qualified Data.Text as T
import AStar
import Data.Char (toLower, isAlphaNum)
import Data.FingerTree.PSQueue (Binding(..))
import Data.List (foldl', maximumBy, partition, sortBy)
import Data.Maybe (maybeToList, fromMaybe, isNothing, fromJust, catMaybes)
import Data.Ord (comparing)
import Control.Applicative ((<*>))
import Control.Monad (when)
import Options.Applicative
import Text.Printf (printf)
import Text.XML.HXT.Core hiding ((:->), when)

--- types ---

data Song = Song {
             songArtist :: T.Text,
             songId :: Int,
             songName :: T.Text,
             songDuration :: Int
            }

instance Eq Song where
  a == b = songId a == songId b

instance Ord Song where
  compare = comparing songId

instance Show Song where
  show (Song {..}) = printf "%s. %s - %s (%sms)"
    (show songId) (T.unpack songArtist) (T.unpack songName) (show songDuration)

data SongLibrary = SongLibrary {
                     songIdMap :: M.Map Int Song,
                     fstCharMap :: M.Map Char [Song],
                     lstCharMap :: M.Map Char [Song]
                   }

data Playlist = Playlist { playlistSongs :: [Song], playlistDuration :: Int }

--- XML parsing ---

atTag tag = deep (isElem >>> hasName tag)

getSong = atTag "Song" >>>
  proc s -> do
    sName <- (T.strip . T.pack) ^<< getAttrValue "name" -< s
    sId <- read ^<< getAttrValue "id" -< s
    sDuration <- read ^<< getAttrValue "duration" -< s
    returnA -< (sId, sName, sDuration)

getSongs = atTag "Artist" >>>
  proc a -> do
    sArtist <- (T.strip . T.pack) ^<< getAttrValue "name" -< a
    songs <- listA getSong -< a
    returnA -< map (uncurry3 $ Song sArtist) songs

getSongsFromXml :: FilePath -> IO SongLibrary
getSongsFromXml file =
  fmap (uncurry3 SongLibrary
        . foldl' (\(mi, mf, ml) s ->
                    (M.insert (songId s) s mi,
                     M.insertWith (++) (fstChar . songName $ s) [s] mf,
                     M.insertWith (++) (lstChar. songName $ s) [s] ml))
                 (M.empty, M.empty, M.empty)
        . concat)
  $ runX (readDocument [withValidate no , withRemoveWS yes] file >>> getSongs)

--- playlist generation ---

cleanSongName =
  T.dropAround (not . isAlphaNum) . head . dropWhile T.null . T.splitOn (T.pack "(")

fstChar = toLower . T.head . cleanSongName
lstChar = toLower . T.last . cleanSongName

nextSongs :: Song -> SongLibrary -> [Song]
nextSongs song = fromMaybe [] . M.lookup (lstChar . songName $ song) . fstCharMap

prevSongs :: Song -> SongLibrary -> [Song]
prevSongs song = fromMaybe [] . M.lookup (fstChar . songName $ song) . lstCharMap

playlist :: SongLibrary -> (Song -> [(Song, Int)]) -> Int -> Int -> Maybe Playlist
playlist library nextSong startId endId = do
  start <- M.lookup startId $ songIdMap library
  end <- M.lookup endId $ songIdMap library
  let pl = concatMap snd . maybeToList . astar start end nextSong $ (\_ _ -> 0)
  return $ Playlist pl (playlistTime pl)

shortestPlaylist, longestPlaylist, shortestTimePlaylist, longestTimePlaylist
  :: SongLibrary -> Int -> Int -> Maybe Playlist

shortestPlaylist library =
  playlist library (\song -> map (\s -> (s, 1)) . nextSongs song $ library)

longestPlaylist library =
  playlist library (\song -> map (\s -> (s, -1)) . nextSongs song $ library)

shortestTimePlaylist library startId =
  playlist library (\song ->
    map (\s -> (s, songDuration . fromJust . M.lookup startId . songIdMap $ library))
    . nextSongs song $ library) startId

longestTimePlaylist library startId =
  playlist library (\song ->
    map (\s -> (s, negate . songDuration . fromJust . M.lookup startId . songIdMap $ library))
    . nextSongs song $ library) startId

playlistTime = sum . map songDuration

playlistTimes :: SongLibrary -> Int -> Int -> M.Map Int Int
playlistTimes library startId endId = let
  songIds = zip (M.keys . songIdMap $ library) (repeat maxBound)
  queue = Q.fromAscList . map (uncurry (:->)) $ songIds
  distances = M.fromAscList songIds
  endSongDuration = songDuration . fromJust . song $ endId

  in M.map (\n -> if n == maxBound then 0 else n)
     $ loop (M.insert endId endSongDuration distances)
            (Q.adjust (const endSongDuration) endId queue)
  where
    song sId = M.lookup sId . songIdMap $ library

    loop distances queue = case Q.findMin queue of
      Nothing -> distances
      Just (sId :-> p) -> let
        queue' = Q.deleteMin queue
        in if p == maxBound
             then distances
             else let
               prev = prevSongs (fromJust $ song sId) library
               (distances', queue'') =
                foldl (\(d, q) Song{..} -> let
                          alt = fromJust (M.lookup sId d) + songDuration
                          old = fromJust $ M.lookup songId d
                          in if alt < old
                               then (M.insert songId alt d, Q.adjust (const alt) songId q)
                               else (d, q))
                      (distances, queue') prev
               in loop distances' queue''

timedPlaylist :: SongLibrary -> Int -> Int -> Int -> Int -> Maybe Playlist
timedPlaylist library time startId endId maxChild =
  fst $ timedPlaylist_ library time startId endId S.empty M.empty
                       (playlistTimes library startId endId) maxChild

timedPlaylist_ library time startId endId seen rejected shortestTimeMap maxChild
 | isNothing song = (Nothing, rejected)
 | startId == endId = (Just $ Playlist [jSong] (songDuration jSong), rejected)
 | fromMaybe 0 (M.lookup startId rejected) > time = (Nothing, rejected)
 | fromMaybe 0 (M.lookup startId shortestTimeMap) > time =
     (Nothing, M.insertWith max startId time rejected)
 | songDuration jSong > time = (Nothing, rejected)
 | otherwise = let
     (rest, rejected', _) =
       foldl
        (\(pls, rej, count) sId -> if count >= maxChild
          then (pls, rej, count)
          else let
            (pl, rej') = timedPlaylist_ library (time - songDuration jSong)
                                        sId endId seen' rej shortestTimeMap maxChild
            in case pl of
             Nothing -> (pl : pls, rej', count)
             Just _ -> (pl : pls, rej', count + 1))
        ([], rejected, 0) nextSongIds
     rest' = catMaybes rest
     in if null rest'
          then if null nextSeen
                 then (Nothing, M.insertWith max startId time rejected')
                 else (Nothing, rejected')
          else let Playlist plSongs plDur = maximumBy (comparing playlistDuration) rest'
               in (Just $ Playlist (jSong : plSongs) (plDur + songDuration jSong), rejected')
     where
       song = M.lookup startId . songIdMap $ library
       jSong = fromJust song
       (nextSongIds, nextSeen) =
        partition (not . flip S.member seen) . map songId
        . sortBy (comparing (negate . songDuration)) . nextSongs jSong $ library
       seen' = S.insert startId seen

--- command line parsing ---

data Command = Shortest Bool
               | Longest Bool
               | Timed Int Int
               deriving (Show)

optParser = subparser $
    command "shortest"
      (info
        (helper
          <*> ((\bt sl st et -> (sl, st, et, Shortest bt))
                <$> byTimeP "Shortest" <*> songLibFileP <*> startIdP <*> endIdP))
        (progDesc "Creates shortest playlist"))
    & command "longest"
      (info
        (helper
          <*> ((\bt sl st et -> (sl, st, et, Longest bt))
                <$> byTimeP "Longest" <*> songLibFileP <*> startIdP <*> endIdP))
        (progDesc "Creates longest playlist"))
    & command "timed"
      (info
        (helper
          <*> ((\sl tm mc st et -> (sl, st, et, Timed tm mc))
                <$> songLibFileP <*> timeP <*> maxChildP <*> startIdP <*> endIdP))
        (progDesc "Creates longest playlist with the maximum specified time"))
  where
    songLibFileP = argument str (metavar "song_library_xml_file")
    byTimeP optimum = switch (short 't' & help (optimum ++ " by playlist time"))
    startIdP = argument auto (metavar "start_song_id")
    endIdP = argument auto (metavar "end_song_id")
    timeP = argument auto (metavar "playlist_time")
    maxChildP = argument auto (metavar "max_children")

opts = info (helper <*> optParser) $
    progDesc "Creates \"Barrel of Monkey\" playlists starting and ending by given songs"

--- main ---

main = do
  (songLibFile, startId, endId, command) <- execParser opts
  library <- getSongsFromXml songLibFile
  when (isNothing . M.lookup startId . songIdMap $ library)
    (error "Start id not found in the library")
  when (isNothing . M.lookup endId . songIdMap $ library)
    (error "End id not found in the library")

  case command of
    Shortest byTime ->
      if byTime
        then printPlaylist $ shortestTimePlaylist library startId endId
        else printPlaylist $ shortestPlaylist library startId endId
    Longest byTime ->
      if byTime
        then printPlaylist $ longestTimePlaylist library startId endId
        else printPlaylist $ longestPlaylist library startId endId
    Timed time maxChild ->
      printPlaylist $ timedPlaylist library time startId endId maxChild
  where
    printPlaylist playlist = case playlist of
      Nothing -> putStrLn "No playlist found"
      Just Playlist{..} -> do
         putStrLn . unlines . map show $ playlistSongs
         putStrLn ("Duration: " ++ show playlistDuration ++ " ms")
