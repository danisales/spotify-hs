{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Secret where

import Network.Wreq
import Control.Lens
import Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Maybe
import Data.ByteString.Base64 as B64
import System.Environment

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

-- Get your credentials at: https://developer.spotify.com/dashboard

-- On terminal:
-- export SPOTIFY_CLIENT_ID="YOUR CLIENT ID"
-- export SPOTIFY_CLIENT_SECRET="YOUR CLIENT SECRET"

clientID :: IO B.ByteString
clientID = do
  id <- getEnv "SPOTIFY_CLIENT_ID"
  return $ B.pack id

clientSecret :: IO B.ByteString
clientSecret = do
  secret <- getEnv "SPOTIFY_CLIENT_SECRET"
  return $ B.pack secret

authToken :: IO B.ByteString
authToken = do
  id <- clientID
  secret <- clientSecret
  return $ B64.encode $ id <> ":" <> secret

getToken :: IO (Maybe Token)
getToken = do
  token <- authToken
  let opts = defaults
             & header "Authorization" .~ ["Basic " <> token]
  r <- postWith opts "https://accounts.spotify.com/api/token" ["grant_type" := ("client_credentials" :: String)]
  return $ (A.decode (r ^?! responseBody) :: Maybe Token)

getAccessToken :: IO String
getAccessToken = do
  token <- getToken
  return $ (extractToken $ fromJust token)
    where
      extractToken Token{access_token} = access_token
