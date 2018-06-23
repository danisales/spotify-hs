{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Control.Lens
import Data.Monoid

data Artist = Artist {
  artist_external_urls :: ExternalURL,
  artist_followers :: Followers,
  artist_genres :: [String],
  artist_href :: String,
  artist_id :: String,
  artist_images :: [Image],
  artist_name :: String,
  artist_popularity :: Integer,
  artist_type :: String,
  artist_uri :: String
} deriving (Show)

instance FromJSON Artist where
  parseJSON = withObject "artist" $ \o ->
    Artist <$> o .: "external_urls"
          <*> o .: "followers"
          <*> o .: "genres"
          <*> o .: "href"
          <*> o .: "id"
          <*> o .: "images"
          <*> o .: "name"
          <*> o .: "popularity"
          <*> o .: "type"
          <*> o .: "uri"

data Artists = Artists {
  artists :: [Artist]
} deriving (Show)

instance FromJSON Artists where
  parseJSON = withObject "artists" $ \o ->
    Artists <$> o .: "artists"

data ExternalURL = ExternalURL {
  spotify :: String
} deriving (Show)

instance FromJSON ExternalURL where
  parseJSON = withObject "externalurl" $ \o ->
    ExternalURL <$> o .: "spotify"

data ExternalID = ExternalID {
  isrc :: String
} deriving (Show)

instance FromJSON ExternalID where
  parseJSON = withObject "externalid" $ \o ->
    ExternalID <$> o .: "isrc"

data Followers = Followers {
  follower_href :: Maybe String,
  follower_total :: Integer
} deriving (Show)

instance FromJSON Followers where
  parseJSON = withObject "followers" $ \o ->
    Followers <$> o .:? "href"
              <*> o .: "total"

data Image = Image {
  img_height :: Maybe Integer,
  img_url :: String,
  img_width :: Maybe Integer
} deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "image" $ \o ->
    Image <$> o .:? "height"
          <*> o .: "url"
          <*> o .:? "width"

data Track = Track {
  track_simplified_album :: SimplifiedAlbum,
  track_simplified_artists :: [SimplifiedArtist],
  track_available_markets :: [String],
  track_disc_number :: Integer,
  track_duration_ms :: Integer,
  track_explict :: Bool,
  track_external_ids :: ExternalID,
  track_external_urls :: ExternalURL,
  track_href :: String,
  track_id :: String,
  track_is_playable :: Maybe Bool,
  track_linked_from :: Maybe String,
  track_restriction :: Maybe Restriction,
  track_name :: String,
  track_popularity :: Integer,
  track_preview_url :: Maybe String,
  track_number :: Integer,
  track_type :: String,
  track_uri :: String
} deriving (Show)

instance FromJSON Track where
  parseJSON = withObject "track" $ \o ->
    Track <$> o .: "album"
          <*> o .: "artists"
          <*> o .: "available_markets"
          <*> o .: "disc_number"
          <*> o .: "duration_ms"
          <*> o .: "explicit"
          <*> o .: "external_ids"
          <*> o .: "external_urls"
          <*> o .: "href"
          <*> o .: "id"
          <*> o .:? "is_playable"
          <*> o .:? "linked_from"
          <*> o .:? "restrictions"
          <*> o .: "name"
          <*> o .: "popularity"
          <*> o .:? "preview_url"
          <*> o .: "track_number"
          <*> o .: "type"
          <*> o .: "uri"

data Tracks = Tracks {
  tracks :: [Track]
} deriving (Show)

instance FromJSON Tracks where
  parseJSON = withObject "tracks" $ \o ->
    Tracks <$> o .: "tracks"

data SimplifiedTracks = SimplifiedTracks {
  s_track_simplified_artists :: [SimplifiedArtist],
  s_track_available_markets :: [String],
  s_track_disc_number :: Integer,
  s_track_duration_ms :: Integer,
  s_track_explict :: Bool,
  s_track_external_urls :: ExternalURL,
  s_track_href :: String,
  s_track_id :: String,
  s_track_is_playable :: Maybe Bool,
  s_track_linked_from :: Maybe String,
  s_track_restriction :: Maybe Restriction,
  s_track_name :: String,
  s_track_preview_url :: Maybe String,
  s_track_number :: Integer,
  s_track_type :: String,
  s_track_uri :: String
} deriving (Show)

instance FromJSON SimplifiedTracks where
  parseJSON = withObject "simplifiedtracks" $ \o ->
    SimplifiedTracks <$> o .: "artists"
          <*> o .: "available_markets"          
          <*> o .: "disc_number"
          <*> o .: "duration_ms"
          <*> o .: "explicit"
          <*> o .: "external_urls"
          <*> o .: "href"
          <*> o .: "id"
          <*> o .:? "is_playable"
          <*> o .:? "linked_from"
          <*> o .:? "restrictions"
          <*> o .: "name"
          <*> o .:? "preview_url"
          <*> o .: "track_number"
          <*> o .: "type"
          <*> o .: "uri"
          
data Restriction = Restriction {
  reason :: String
} deriving (Show)

instance FromJSON Restriction where
  parseJSON = withObject "restriction" $ \o ->
    Restriction <$> o .: "reason"

data SimplifiedArtist = SimplifiedArtist {
  s_artist_external_urls :: ExternalURL,
  s_artist_href :: String,
  s_artist_id :: String,
  s_artist_name :: String,
  s_artist_type :: String,
  s_artist_uri :: String
} deriving (Show)

instance FromJSON SimplifiedArtist where
  parseJSON = withObject "simplifiedartist" $ \o ->
    SimplifiedArtist <$> o .: "external_urls"
          <*> o .: "href"
          <*> o .: "id"
          <*> o .: "name"
          <*> o .: "type"
          <*> o .: "uri"

data SimplifiedArtists = SimplifiedArtists {
  s_artists :: [SimplifiedArtist]
} deriving (Show)

data SimplifiedAlbum = SimplifiedAlbum {
  s_album_albumgroup :: Maybe String,
  s_album_albumtype :: String,
  s_album_artists :: [SimplifiedArtist],
  s_album_available_markets :: [String],
  s_album_external_urls :: ExternalURL,
  s_album_href :: String,
  s_album_id :: String,
  s_album_images :: [Image],
  s_album_name :: String,
  s_album_release_date :: String,
  s_album_release_date_precision :: String,
  s_album_restrictions :: Maybe Restriction,
  s_album_type :: String,
  s_album_uri :: String
} deriving (Show)

instance FromJSON SimplifiedAlbum where
  parseJSON = withObject "simplifiedalbum" $ \o ->
    SimplifiedAlbum <$> o .:? "album_group"
          <*> o .: "album_type"
          <*> o .: "artists"
          <*> o .: "available_markets"
          <*> o .: "external_urls"
          <*> o .: "href"
          <*> o .: "id"
          <*> o .: "images"
          <*> o .: "name"
          <*> o .: "release_date"
          <*> o .: "release_date_precision"
          <*> o .:? "restrictions"
          <*> o .: "type"
          <*> o .: "uri"

data SimplifiedAlbums = SimplifiedAlbums {
  s_albums :: [SimplifiedAlbum]
} deriving (Show)

data AudioFeatures = AudioFeatures {
  af_acousticness :: Double,
  af_analysis_url :: String,
  af_danceability :: Double,
  af_duration_ms :: Integer,
  af_energy :: Double,
  af_id :: String,
  af_instrumentalness :: Double,
  af_key :: Integer,
  af_liveness :: Double,
  af_loudness :: Double,
  af_mode :: Integer,
  af_speechiness :: Double,
  af_tempo :: Double,
  af_time_signature :: Integer,
  af_track_href :: String,
  af_type :: String,
  af_uri :: String,
  af_valence :: Double
} deriving (Show)

instance FromJSON AudioFeatures where
  parseJSON = withObject "audiofeatures" $ \o ->
    AudioFeatures <$> o .: "acousticness"
          <*> o .: "analysis_url"
          <*> o .: "danceability"
          <*> o .: "duration_ms"
          <*> o .: "energy"
          <*> o .: "id"
          <*> o .: "instrumentalness"
          <*> o .: "key"
          <*> o .: "liveness"
          <*> o .: "loudness"
          <*> o .: "mode"
          <*> o .: "speechiness"
          <*> o .: "tempo"
          <*> o .: "time_signature"
          <*> o .: "track_href"
          <*> o .: "type"
          <*> o .: "uri"
          <*> o .: "valence"

data MultipleAudioFeatures = MultipleAudioFeatures {
  audio_features :: [AudioFeatures]
} deriving (Show)

instance FromJSON MultipleAudioFeatures where
  parseJSON = withObject "multaudiofeatures" $ \o ->
    MultipleAudioFeatures <$> o .: "audio_features"

data ArtistAlbums = ArtistAlbums {
  albums :: [SimplifiedAlbum]
} deriving (Show)

instance FromJSON ArtistAlbums where
  parseJSON = withObject "artistalbums" $ \o ->
    ArtistAlbums <$> o .: "items"

data AlbumTracks = AlbumTracks {
  at_tracks :: [SimplifiedTracks]
} deriving (Show)

instance FromJSON AlbumTracks where
  parseJSON = withObject "albumtracks" $ \o ->
    AlbumTracks <$> o .: "items"
