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
import ConvertTypes

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
  return $ (A.decode (r ^?! responseBody) :: Maybe Artist)

-- e.g. getArtists ["6yz8H2Aks1bHaKNiHCutaR", "4ERtgeBbWRkFzIz6LaFCeY", "3yY2gUcIsjMr8hjo51PoJ8"]
getArtists :: [String] -> IO (Maybe [Artist])
getArtists ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists?ids=" <> intercalate "," ids)
  return $ fromArtists (A.decode (r ^?! responseBody) :: Maybe Artists)

-- e.g. getArtistAlbums "6yz8H2Aks1bHaKNiHCutaR" "BR"
getArtistAlbums :: String -> String -> IO (Maybe [SimplifiedAlbum])
getArtistAlbums id market = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/albums?limit=50&market=" <> market)
  return $ fromArtistAlbums (A.decode (r ^?! responseBody) :: Maybe ArtistAlbums)

-- e.g. getRelatedArtists "6yz8H2Aks1bHaKNiHCutaR"
getRelatedArtists :: String -> IO (Maybe [Artist])
getRelatedArtists id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/related-artists")
  return $ fromArtists (A.decode (r ^?! responseBody) :: Maybe Artists)

-- e.g. getTopTracks "6yz8H2Aks1bHaKNiHCutaR" "BR"
getTopTracks :: String -> String -> IO (Maybe [Track])
getTopTracks id country = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/artists/" <> id <> "/top-tracks?country=" <> country)
  return $ fromTracks (A.decode (r ^?! responseBody) :: Maybe Tracks)

-- e.g. getTrack "0WQiDwKJclirSYG9v5tayI"
getTrack :: String -> IO (Maybe Track)
getTrack id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/tracks/" <> id)
  return $ (A.decode (r ^?! responseBody) :: Maybe Track)

-- e.g. getTracks ["2hopb3OJKYhhZ6L4ca9pBp", "3SVAN3BRByDmHOhKyIDxfC", "0WQiDwKJclirSYG9v5tayI"]
getTracks :: [String] -> IO (Maybe [Track])
getTracks ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/tracks?ids=" <> intercalate "," ids)
  return $ fromTracks (A.decode (r ^?! responseBody) :: Maybe Tracks)

-- e.g. getAudioFeatures "0WQiDwKJclirSYG9v5tayI"
getAudioFeatures :: String -> IO (Maybe AudioFeatures)
getAudioFeatures id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/audio-features/" <> id)
  return $ (A.decode (r ^?! responseBody) :: Maybe AudioFeatures)

-- e.g. getMultipleAudioFeatures ["2hopb3OJKYhhZ6L4ca9pBp", "3SVAN3BRByDmHOhKyIDxfC", "0WQiDwKJclirSYG9v5tayI"]
getMultipleAudioFeatures :: [String] -> IO (Maybe [AudioFeatures])
getMultipleAudioFeatures ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/audio-features?ids=" <> intercalate "," ids)
  return $ fromMultipleAudioFeatures (A.decode (r ^?! responseBody) :: Maybe MultipleAudioFeatures)

-- e.g. getAlbum "2AwtTIdUFaUl69alioeFut"
getAlbum :: String -> IO (Maybe Album)
getAlbum id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums/" <> id)
  return $ (A.decode (r ^?! responseBody) :: Maybe Album)

-- e.g. getAlbums ["6GphKx2QAPRoVGWE9D7ou8" ,"5Y0p2XCgRRIjna91aQE8q7", "2AwtTIdUFaUl69alioeFut"]
getAlbums :: [String] -> IO (Maybe [Album])
getAlbums ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums?ids=" <> intercalate "," ids)
  return $ fromAlbums (A.decode (r ^?! responseBody) :: Maybe Albums)

-- e.g. getAlbumTracks "2AwtTIdUFaUl69alioeFut"
getAlbumTracks :: String -> IO (Maybe [SimplifiedTrack])
getAlbumTracks id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/albums/" <> id <> "/tracks?limit=50")
  return $ fromAlbumTracks (A.decode (r ^?! responseBody) :: Maybe AlbumTracks)

-- e.g. getUser "spotify"
getUser :: String -> IO (Maybe User)
getUser id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> id)
  return $ (A.decode (r ^?! responseBody) :: Maybe User)

-- e.g. getUserPlaylists "spotify"
getUserPlaylists :: String -> IO (Maybe [SimplifiedPlaylist])
getUserPlaylists id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> id <> "/playlists?limit=50")
  return $ fromUserPlaylists (A.decode (r ^?! responseBody) :: Maybe UserPlaylists)

-- e.g. getCategory "indie_alt"
getCategory :: String -> IO (Maybe Category)
getCategory id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/categories/" <> id)
  return $ (A.decode (r ^?! responseBody) :: Maybe Category)

-- e.g. getCategoryPlaylists "indie_alt"
getCategoryPlaylists :: String -> IO (Maybe [SimplifiedPlaylist])
getCategoryPlaylists id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/categories/" <> id <> "/playlists?limit=50")
  return $ fromPlaylists (A.decode (r ^?! responseBody) :: Maybe Playlists)

-- e.g. getFeaturedPlaylists
getFeaturedPlaylists :: IO (Maybe [SimplifiedPlaylist])
getFeaturedPlaylists = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/featured-playlists?limit=50")
  return $ fromPlaylists (A.decode (r ^?! responseBody) :: Maybe Playlists)

-- e.g. getListCategories
getListCategories :: IO (Maybe [Category])
getListCategories = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/categories?limit=50")
  return $ fromCategories (A.decode (r ^?! responseBody) :: Maybe Categories)

-- e.g. getNewReleases "BR"
getNewReleases :: String -> IO (Maybe [SimplifiedAlbum])
getNewReleases country = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/browse/new-releases?country=" <> country)
  return $ fromAlbumsNewReleases (A.decode (r ^?! responseBody) :: Maybe AlbumsNewReleases)

-- e.g. getAvailableGenreSeeds
getAvailableGenreSeeds :: IO (Maybe Genres)
getAvailableGenreSeeds = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/recommendations/available-genre-seeds")
  return $ (A.decode (r ^?! responseBody) :: Maybe Genres)

-- e.g. getPlaylist "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylist :: String -> String -> IO (Maybe Playlist)
getPlaylist user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist)
  return $ (A.decode (r ^?! responseBody) :: Maybe Playlist)

-- e.g. getPlaylistTracks "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylistTracks :: String -> String -> IO (Maybe [PlaylistTrack])
getPlaylistTracks user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist <> "/tracks")
  return $ fromPlaylistTracks (A.decode (r ^?! responseBody) :: Maybe PlaylistTracks)

-- e.g. getPlaylistCoverImg "spotify" "37i9dQZF1DXbMYUPb05hjJ"
getPlaylistCoverImg :: String -> String -> IO (Maybe [Image])
getPlaylistCoverImg user playlist = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts (baseUrl <> "/users/" <> user <> "/playlists/" <> playlist <> "/images")
  return $ (A.decode (r ^?! responseBody) :: Maybe [Image])
