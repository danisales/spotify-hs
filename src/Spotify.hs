{-# LANGUAGE OverloadedStrings #-}

module Spotify where

import Network.Wreq
import Control.Lens
import Data.Aeson as A
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Maybe
import Data.ByteString.Lazy.Internal
import Data.List
import Secret
import Types
import Data.Map (Map)

baseUrl :: String
baseUrl = "https://api.spotify.com/v1"

-- e.g. getArtist "6yz8H2Aks1bHaKNiHCutaR"
getArtist :: String -> IO (Maybe Artist)
getArtist id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Artist)

-- e.g. getArtists ["6yz8H2Aks1bHaKNiHCutaR", "4ERtgeBbWRkFzIz6LaFCeY", "3yY2gUcIsjMr8hjo51PoJ8"]
getArtists :: [String] -> IO (Maybe Artists)
getArtists ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists?ids=" <> intercalate "," ids)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Artists)

-- e.g. getArtistAlbums "6yz8H2Aks1bHaKNiHCutaR" "BR"
getArtistAlbums :: String -> String -> IO (Maybe ArtistAlbums)
getArtistAlbums id market = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/albums?limit=50&market=" <> market)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe ArtistAlbums)

-- e.g. getRelatedArtists "6yz8H2Aks1bHaKNiHCutaR"
getRelatedArtists :: String -> IO (Maybe Artists)
getRelatedArtists id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/related-artists")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Artists)

-- e.g. getTopTracks "6yz8H2Aks1bHaKNiHCutaR" "BR"
getTopTracks :: String -> String -> IO (Maybe Tracks)
getTopTracks id country = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/top-tracks?country=" <> country)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Tracks)

-- e.g. getTrack "0WQiDwKJclirSYG9v5tayI"
getTrack :: String -> IO (Maybe Track)
getTrack id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/tracks/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Track)

-- e.g. getTracks ["2hopb3OJKYhhZ6L4ca9pBp", "3SVAN3BRByDmHOhKyIDxfC", "0WQiDwKJclirSYG9v5tayI"]
getTracks :: [String] -> IO (Maybe Tracks)
getTracks ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/tracks?ids=" <> intercalate "," ids)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Tracks)

-- e.g. getAudioFeatures "0WQiDwKJclirSYG9v5tayI"
getAudioFeatures :: String -> IO (Maybe AudioFeatures)
getAudioFeatures id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/audio-features/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe AudioFeatures)

-- e.g. getMultipleAudioFeatures ["2hopb3OJKYhhZ6L4ca9pBp", "3SVAN3BRByDmHOhKyIDxfC", "0WQiDwKJclirSYG9v5tayI"]
getMultipleAudioFeatures :: [String] -> IO (Maybe MultipleAudioFeatures)
getMultipleAudioFeatures ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/audio-features/?ids=" <> intercalate "," ids)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe MultipleAudioFeatures)

-- e.g. getAlbum "2AwtTIdUFaUl69alioeFut"
getAlbum :: String -> IO (Maybe Album)
getAlbum id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Album)

-- e.g. getAlbums ["6GphKx2QAPRoVGWE9D7ou8" ,"5Y0p2XCgRRIjna91aQE8q7", "2AwtTIdUFaUl69alioeFut"]
getAlbums :: [String] -> IO (Maybe Albums)
getAlbums ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums?ids=" <> intercalate "," ids)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Albums)

-- e.g. getAlbumTracks "2AwtTIdUFaUl69alioeFut"
getAlbumTracks :: String -> IO (Maybe AlbumTracks)
getAlbumTracks id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums/" <> id <> "/tracks?limit=50")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe AlbumTracks)

-- e.g. getUser "spotify"
getUser :: String -> IO (Maybe User)
getUser id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe User)

-- e.g. getCategory "indie_alt"
getCategory :: String -> IO (Maybe Category)
getCategory id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/categories/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Category)

-- e.g. getListCategories
getListCategories :: IO (Maybe Categories)
getListCategories = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/categories?limit=50")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Categories)

-- e.g. getNewReleases "BR"
getNewReleases :: String -> IO (Maybe AlbumsNewReleases)
getNewReleases country = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/new-releases?country=" <> country)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe AlbumsNewReleases)

-- e.g. getAvailableGenreSeeds
getAvailableGenreSeeds :: IO (Maybe Genres)
getAvailableGenreSeeds = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/recommendations/available-genre-seeds")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Genres)

-- e.g. getPlaylist "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylist :: String -> String -> IO (Maybe Playlist)
getPlaylist user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Playlist)

-- e.g. getPlaylistTracks "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylistTracks :: String -> String -> IO (Maybe PlaylistTracks)
getPlaylistTracks user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist <> "/tracks")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe PlaylistTracks)

-- e.g. getPlaylistCoverImg "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylistCoverImg :: String -> String -> IO (Maybe [Image])
getPlaylistCoverImg user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist <> "/images")
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe [Image])
