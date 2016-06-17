{-# LANGUAGE OverloadedStrings #-}

module Herokfred.Cache
    ( getApps
    ) where

import Control.Exception (throwIO)
import Control.Monad (mzero)
import Data.Monoid ((<>))
import System.Environment (getEnv)

import Control.Lens ((.~), (^.), (&))
import Data.Aeson
import LoadEnv (loadEnv)
import Network.Wreq
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Alfred as A

newtype Token = Token String

data HerokuApp = HerokuApp
    { appName :: T.Text
    , appUrl :: T.Text
    } deriving (Show)

instance FromJSON HerokuApp where
    parseJSON (Object v) = do
        name <- v .: "name"

        return $ HerokuApp name $ dashboardUrl name

    parseJSON _ = mzero

instance A.ToAlfredItem HerokuApp where
    toAlfredItem (HerokuApp name url) = A.Item name url 0

getApps :: IO [HerokuApp]
getApps = requestApps =<< getToken

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

dashboardUrl :: T.Text -> T.Text
dashboardUrl = mappend "https://dashboard.heroku.com/apps/"
