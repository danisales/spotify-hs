module ConvertTypes where

import Types
import Data.Maybe

fromArtists :: Maybe Artists -> Maybe [Artist]
fromArtists Nothing = Nothing
fromArtists (Just (Artists a)) = Just a

fromArtistAlbums :: Maybe ArtistAlbums -> Maybe [SimplifiedAlbum]
fromArtistAlbums Nothing = Nothing
fromArtistAlbums (Just (ArtistAlbums a)) = Just a

fromTracks :: Maybe Tracks -> Maybe [Track]
fromTracks Nothing = Nothing
fromTracks (Just (Tracks a)) = Just a

fromMultipleAudioFeatures :: Maybe MultipleAudioFeatures -> Maybe [AudioFeatures]
fromMultipleAudioFeatures Nothing = Nothing
fromMultipleAudioFeatures (Just (MultipleAudioFeatures a)) = Just a

fromAlbums :: Maybe Albums -> Maybe [Album]
fromAlbums Nothing = Nothing
fromAlbums (Just (Albums a)) = Just a

fromAlbumTracks :: Maybe AlbumTracks -> Maybe [SimplifiedTracks]
fromAlbumTracks Nothing = Nothing
fromAlbumTracks (Just (AlbumTracks a)) = Just a

fromAlbumsNewReleases :: Maybe AlbumsNewReleases -> Maybe [SimplifiedAlbum]
fromAlbumsNewReleases Nothing = Nothing
fromAlbumsNewReleases (Just (AlbumsNewReleases a)) = Just a

fromPlaylists :: Maybe Playlists -> Maybe [SimplifiedPlaylist]
fromPlaylists Nothing = Nothing
fromPlaylists (Just (Playlists a)) = Just a

fromUserPlaylists :: Maybe UserPlaylists -> Maybe [SimplifiedPlaylist]
fromUserPlaylists Nothing = Nothing
fromUserPlaylists (Just (UserPlaylists a)) = Just a

fromCategories :: Maybe Categories -> Maybe [Category]
fromCategories Nothing = Nothing
fromCategories (Just (Categories a)) = Just a

fromPlaylistTracks :: Maybe PlaylistTracks -> Maybe [PlaylistTrack]
fromPlaylistTracks Nothing = Nothing
fromPlaylistTracks (Just (PlaylistTracks a)) = Just a
