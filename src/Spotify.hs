{-# LANGUAGE OverloadedStrings #-}

module Spotify
( AccessToken(..)
, TrackUri
, PlaylistId(..)
, Warning(..)
, warningAlbum
, getAlbumTracks
, addTracksToPlaylist)
where

import Control.Monad (forM, forM_, void)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit (applyBearerAuth, setQueryString)
import WMCE

apiRoot :: String
apiRoot = "https://api.spotify.com/v1"

-- AUTHORIZATION

newtype AccessToken = AccessToken { fromAccessToken :: Text }

parseAuthenticatedRequest :: AccessToken -> String -> IO Request
parseAuthenticatedRequest token url = do
  req <- parseRequestThrow url
  pure $ applyBearerAuth (Text.encodeUtf8 $ fromAccessToken token) req


-- ALBUM SEARCH

newtype AlbumId = AlbumId { fromAlbumId :: Text }

newtype SearchResponse = SearchResponse
  { searchResponseFirstAlbumId :: Maybe AlbumId
  }

instance FromJSON SearchResponse where
  parseJSON :: Value -> Parser SearchResponse
  parseJSON = withObject "response" $ \v -> do
    albums <- v .: "albums"
    flip (withObject "albums") albums $ \v -> do
      items <- v .: "items"
      flip (withArray "items") items $ \a -> do
        searchResponseFirstAlbumId <- forM (a !? 0) $ \v ->
          flip (withObject "album") v $ \v ->
            AlbumId <$> v .: "id"
        pure SearchResponse { searchResponseFirstAlbumId }

searchQueryString :: Album -> Text
searchQueryString album =
  albumTitle album <> " " <> Text.intercalate " " (albumArtists album)

searchAlbumsReq :: AccessToken -> Album -> IO Request
searchAlbumsReq tk album = do
  req <- parseAuthenticatedRequest tk $ apiRoot ++ "/search"
  pure $ setQueryString
    [ ("q", Just $ Text.encodeUtf8 $ searchQueryString album)
    , ("type", Just "album"), ("limit", Just "1") ]
    req

searchAlbum :: AccessToken -> Album -> IO (Maybe AlbumId)
searchAlbum tk album = do
  req <- searchAlbumsReq tk album
  searchResponseFirstAlbumId . getResponseBody <$> httpJSON req


-- ALBUM LOOKUP

newtype TrackUri = TrackUri { fromTrackUri :: Text }

data AlbumResponse = AlbumResponse
  { albumResponseName :: Text
  , albumResponseArtistNames :: Vector Text
  , albumResponseTrackUris :: Vector TrackUri
  }

instance FromJSON AlbumResponse where
  parseJSON :: Value -> Parser AlbumResponse
  parseJSON = withObject "album" $ \v -> do
    name <- v .: "name"
    albumResponseName <- withText "name" pure name
    artists <- v .: "artists"
    albumResponseArtistNames <- flip (withArray "artists") artists $ \a ->
      forM a $ \artist ->
        flip (withObject "artist") artist $ \v -> do
          name <- v .: "name"
          withText "name" pure name
    tracks <- v .: "tracks"
    albumResponseTrackUris <- flip (withObject "tracks") tracks $ \v -> do
      items <- v .: "items"
      flip (withArray "items") items $ \a ->
        forM a $ \item ->
          flip (withObject "item") item $ \v -> do
            uri <- v .: "uri"
            withText "uri" (pure . TrackUri) uri
    pure AlbumResponse {
      albumResponseName, albumResponseArtistNames, albumResponseTrackUris
    }

albumReq :: AccessToken -> AlbumId -> IO Request
albumReq tk id = parseAuthenticatedRequest tk $
  apiRoot ++ "/albums/" ++ Text.unpack (fromAlbumId id)

getAlbum :: AccessToken -> AlbumId -> IO AlbumResponse
getAlbum tk id = do
  req <- albumReq tk id
  getResponseBody <$> httpJSON req

data Warning
  = AlbumNotFound Album
  | AlbumTitleMismatch Album Text
  | AlbumArtistMismatch Album (Vector Text)

warningAlbum :: Warning -> Album
warningAlbum (AlbumNotFound album) = album
warningAlbum (AlbumTitleMismatch album _) = album
warningAlbum (AlbumArtistMismatch album _) = album

validateAlbum :: Album -> AlbumResponse -> Maybe Warning
validateAlbum album resp
  | Text.toCaseFold (albumResponseName resp) /=
    Text.toCaseFold (albumTitle album)
    = Just $ AlbumTitleMismatch album (albumResponseName resp)
  | Set.fromList
      (map Text.toCaseFold $ Vector.toList $ albumResponseArtistNames resp) /=
    Set.fromList (map Text.toCaseFold $ albumArtists album)
    = Just $ AlbumArtistMismatch album (albumResponseArtistNames resp)
  | otherwise = Nothing

getAlbumTracks :: AccessToken -> Album -> IO (Vector TrackUri, Maybe Warning)
getAlbumTracks tk album = do
  albumIdM <- searchAlbum tk album
  case albumIdM of
    Just albumId -> do
      albumResp <- getAlbum tk albumId
      let warning = validateAlbum album albumResp
      pure (albumResponseTrackUris albumResp, warning)
    Nothing -> pure (Vector.empty, Just $ AlbumNotFound album)


-- ADDING TRACKS TO PLAYLIST

newtype PlaylistId = PlaylistId { fromPlaylistId :: Text }

newtype AddTracksToPlaylistReq = AddTracksToPlaylistReq
  { trackUris :: Vector TrackUri
  }

instance ToJSON AddTracksToPlaylistReq where
  toJSON :: AddTracksToPlaylistReq -> Value
  toJSON AddTracksToPlaylistReq { trackUris } =
    let uris = fmap fromTrackUri trackUris in
    object ["uris" .= uris]

addTracksToPlaylistReq ::
  AccessToken -> PlaylistId -> Vector TrackUri -> IO Request
addTracksToPlaylistReq tk playlistId trackIds = do
  req <- parseAuthenticatedRequest tk $
    "POST " ++ apiRoot ++ "/playlists/" ++
    Text.unpack (fromPlaylistId playlistId) ++ "/tracks"
  pure $ setRequestBodyJSON (AddTracksToPlaylistReq trackIds) req

chunkVector :: Int -> Vector a -> [Vector a]
chunkVector n v =
  if Vector.length v <= n then
    [v]
  else
    let (chunk, rest) = Vector.splitAt n v in
    chunk : chunkVector n rest

addTracksToPlaylist :: AccessToken -> PlaylistId -> Vector TrackUri -> IO ()
addTracksToPlaylist tk playlistId trackUris =
  forM_ (chunkVector 100 trackUris) $ \uris -> do
    req <- addTracksToPlaylistReq tk playlistId uris
    void $ httpNoBody req
