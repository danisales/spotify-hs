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

-- e.g. getArtist "6yz8H2Aks1bHaKNiHCutaR"
getArtist :: String -> IO (Maybe Artist)
getArtist id = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts ("https://api.spotify.com/v1/artists/" <> id)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Artist)

-- e.g. getArtists ["6yz8H2Aks1bHaKNiHCutaR", "4ERtgeBbWRkFzIz6LaFCeY", "3yY2gUcIsjMr8hjo51PoJ8"]
getArtists :: [String] -> IO (Maybe Artists)
getArtists ids = do
  token <- getAccessToken
  let opts = defaults & param "Accept" .~ ["application/json"]
                      & param "Content-Type" .~["application/json"]
                      & header "Authorization" .~ ["Bearer " <> B.pack token]
  r <- getWith opts ("https://api.spotify.com/v1/artists?ids=" <> intercalate "," ids)
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Artists)
