import Spotify
import Types
import ConvertTypes
import Data.Maybe

idSimpleAlbum :: SimplifiedAlbum -> String
idSimpleAlbum (SimplifiedAlbum _ _ _ _ _ _ id _ _ _ _ _ _ _) = id

firstTrack :: Album -> SimplifiedTrack
firstTrack (Album _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ t _ _) = head (fromJust (fromAlbumTracks (Just t)))

albumTracks :: Album -> [SimplifiedTrack]
albumTracks (Album _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ t _ _) = fromJust (fromAlbumTracks (Just t))

trackId :: SimplifiedTrack -> String
trackId (SimplifiedTrack _ _ _ _ _ _ _ id _ _ _ _ _ _ _ _) = id

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