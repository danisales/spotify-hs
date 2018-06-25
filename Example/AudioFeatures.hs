import Spotify
import Types
import ConvertTypes
import Data.Maybe

idSimpleAlbum :: SimplifiedAlbum -> String
idSimpleAlbum (SimplifiedAlbum _ _ _ _ _ _ id _ _ _ _ _ _ _) = id

firstTrack :: Album -> SimplifiedTracks
firstTrack (Album _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ t _ _) = head (fromJust (fromAlbumTracks (Just t)))

trackId :: SimplifiedTracks -> String
trackId (SimplifiedTracks _ _ _ _ _ _ _ id _ _ _ _ _ _ _ _) = id

main :: IO ()
main = do
  albums <- getArtistAlbums "6yz8H2Aks1bHaKNiHCutaR" "BR"
  let firstAlbumId = idSimpleAlbum (head $ fromJust albums)
  album <- getAlbum firstAlbumId
  let fstTrack = trackId (firstTrack (fromJust album))
  audioFeatures <- getAudioFeatures fstTrack

  print $ (fromJust audioFeatures)
  
  return ()