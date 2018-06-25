module Main where

import Spotify
import Data.Maybe

main :: IO ()
main = do
  a <- getArtist "6yz8H2Aks1bHaKNiHCutaR"
  print $ fromJust a
  return ()
