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

data Followers = Followers {
  follower_href :: Maybe String,
  follower_total :: Integer
} deriving (Show)

instance FromJSON Followers where
  parseJSON = withObject "followers" $ \o ->
    Followers <$> o .:? "href"
              <*> o .: "total"

data Image = Image {
  img_height :: Integer,
  img_url :: String,
  img_width :: Integer
} deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "followers" $ \o ->
    Image <$> o .: "height"
          <*> o .: "url"
          <*> o .: "width"
