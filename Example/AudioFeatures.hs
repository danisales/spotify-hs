{-# LANGUAGE NamedFieldPuns #-}

module AudioFeatures where

import Spotify
import Types
import ConvertTypes
import Data.Maybe

idSimpleAlbum :: SimplifiedAlbum -> String
idSimpleAlbum SimplifiedAlbum {s_album_id} = s_album_id

firstTrack :: Album -> SimplifiedTrack
firstTrack Album{album_tracks} = head (fromJust (fromAlbumTracks (Just album_tracks)))

albumTracks :: Album -> [SimplifiedTrack]
albumTracks Album{album_tracks} = fromJust (fromAlbumTracks (Just album_tracks))

trackId :: SimplifiedTrack -> String
trackId SimplifiedTrack{s_track_id} = s_track_id

main :: IO ()
main = do
  let artistId = "3yY2gUcIsjMr8hjo51PoJ8"

  artist <- getArtist artistId
  print $ "Artist"
  print $ artist

  albums <- getArtistAlbums artistId "BR"
  let firstAlbumId = idSimpleAlbum (head $ fromJust albums)
  album <- getAlbum firstAlbumId
  print $ "Album"
  print $ album

  let fstTrack = trackId (firstTrack (fromJust album))
  audioFeatures <- getAudioFeatures fstTrack

  print $ "Audio Features"
  print $ (fromJust audioFeatures)

  print $  "Track ids"
  let tracks = map trackId (albumTracks (fromJust album))
  print $ tracks

  print $  "Multiple Audio Features"
  multFeatures <- getMultipleAudioFeatures tracks
  print $ multFeatures

  return ()