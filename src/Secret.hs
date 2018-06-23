{-# LANGUAGE OverloadedStrings #-}

module Secret where

import Network.Wreq
import Control.Lens
import Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Maybe
import Data.ByteString.Base64 as B64

data Token = Token {access_token :: String,
                    token_type :: String,
                    expires_in :: Integer,
                    scope :: String}
  deriving (Show)

instance FromJSON Token where
  parseJSON = withObject "token" $ \o ->
    Token <$> o .: "access_token"
          <*> o .: "token_type"
          <*> o .: "expires_in"
          <*> o .: "scope"

clientID :: B.ByteString
clientID = B.pack "SPOTIFY_CLIENT_ID"

clientSecret :: B.ByteString
clientSecret = B.pack "SPOTIFY_CLIENT_SECRET"

authToken :: B.ByteString
authToken = B64.encode $ clientID <> ":" <> clientSecret

getToken' = do
  let opts = defaults
             & header "Authorization" .~ ["Basic " <> authToken]
  r <- postWith opts "https://accounts.spotify.com/api/token" ["grant_type" := ("client_credentials" :: String)]
  return $ r ^? responseBody


getToken :: IO (Maybe Token)
getToken = do
  let opts = defaults
             & header "Authorization" .~ ["Basic " <> authToken]
  r <- postWith opts "https://accounts.spotify.com/api/token" ["grant_type" := ("client_credentials" :: String)]
  return $ (A.decode (fromJust $ r ^? responseBody) :: Maybe Token)

getAccessToken :: IO String
getAccessToken = do
  token <- getToken
  return $ (extractToken $ fromJust token)
    where
      extractToken (Token a _ _ _) = a
