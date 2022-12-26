{-# LANGUAGE OverloadedStrings#-}

module Main (main) where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector ((!))
import Data.Vector qualified as Vector
import Spotify
import System.Environment (getArgs)
import System.Exit (die)
import System.Random.Stateful
import WMCE (Album(..), parseCharts)

getRandomTrack :: AccessToken -> Album -> IO (Maybe TrackUri, Maybe Warning)
getRandomTrack tk album = do
  (tracks, warning) <- getAlbumTracks tk album
  if null tracks then
    pure (Nothing, warning)
  else do
    num <- uniformRM (0, Vector.length tracks - 1) globalStdGen
    pure (Just $ tracks ! num, warning)

getRandomTracks :: AccessToken -> [Album] -> IO ([TrackUri], [Warning])
getRandomTracks tk albums = do
  tracksAndWarnings <- mapM (getRandomTrack tk) albums
  let trackIds = mapMaybe fst tracksAndWarnings
  let warnings = mapMaybe snd tracksAndWarnings
  pure (trackIds, warnings)

renderWarning :: Warning -> String
renderWarning w =
  let prefix =
        "[WARN] For album '" ++ Text.unpack (albumTitle $ warningAlbum w) ++
        "': "
  in
  prefix ++ case w of
    AlbumNotFound _ -> "album not found"
    AlbumTitleMismatch album title ->
      "title mismatch: expected '" ++ Text.unpack (albumTitle album) ++
      "' but got '" ++ Text.unpack title ++ "'"
    AlbumArtistMismatch album artists ->
      "artists mismatch: expected " ++ show (albumArtists album) ++
      " but got " ++ show artists

addRandomTracksToPlaylist :: AccessToken -> PlaylistId -> [Album] -> IO ()
addRandomTracksToPlaylist tk playlistId albums = do
  (trackIds, warnings) <- getRandomTracks tk albums
  forM_ warnings (putStrLn . renderWarning)
  addTracksToPlaylist tk playlistId (Vector.fromList trackIds)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, playlistId, tk] -> do
      input <- Text.readFile fname
      let albumsE = parseCharts input
      case albumsE of
        Left err -> putStrLn err
        Right albums -> do
          let tk' = AccessToken $ Text.pack tk
          let playlistId' = PlaylistId $ Text.pack playlistId
          addRandomTracksToPlaylist tk' playlistId' albums
    _ -> die "Usage: wmce-playlist <file> <playlist_id> <oauth2_token>"
