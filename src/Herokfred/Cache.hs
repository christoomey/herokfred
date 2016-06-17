{-# LANGUAGE OverloadedStrings #-}

module Herokfred.Cache
    ( cacheApps
    ) where

import Control.Exception (throwIO)
import Control.Monad (mzero)
import Data.Monoid ((<>))
import System.Environment (getEnv)

import Control.Lens ((.~), (^.), (&))
import Data.Aeson
import LoadEnv (loadEnv)
import Network.Wreq
import qualified Data.ByteString.Char8 as B

newtype Token = Token String

data HerokuApp = HerokuApp
    { appId :: String
    , appName :: String
    , appUrl :: String
    } deriving (Show)

instance FromJSON HerokuApp where
    parseJSON (Object v) = do
        id <- v .: "id"
        name <- v .: "name"

        return $ HerokuApp id name ( dashboardUrl name )

    parseJSON _ = mzero

cacheApps :: IO ()
cacheApps = do
  apps <- requestApps =<< getToken
  mapM_ (putStrLn . show) apps

requestApps :: Token -> IO [HerokuApp]
requestApps token = do
  r <- getWith (opts token) endpoint
  let json = r ^. responseBody
  let result = eitherDecode json

  case result of
    Left e -> throwIO (userError e)
    Right apps -> return apps

opts :: Token -> Options
opts (Token t) =
    defaults
        & header "Authorization" .~ ["Bearer " <> B.pack t]
        & header "Accept" .~ ["application/vnd.heroku+json; version=3"]

getToken :: IO Token
getToken = do
  loadEnv
  Token <$> getEnv "HEROKU_API_TOKEN"

endpoint :: String
endpoint = "https://api.heroku.com/apps"

dashboardUrl :: String -> String
dashboardUrl = mappend "https://dashboard.heroku.com/apps/"
