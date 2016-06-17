module Main where

import Options.Applicative
import qualified Alfred as A
import qualified Data.Text as T

import Herokfred.Cache

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Interact with Heroku API")


data Command
    = Search (Maybe String)
    | Cache
    | Increment String


run :: Command -> IO ()
run cmd =
    case cmd of
        Search mquery -> A.searchItems cacheFile mquery
        Cache -> do
            apps <- getApps
            A.writeItems cacheFile $ fmap A.toAlfredItem apps
        Increment url -> A.incrementVisits cacheFile $ T.pack url

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
    subparser $
        command "search" (parseSearch `withInfo` "Search for Heroku apps") <>
        command "cache" (parseCache  `withInfo` "Cache Heroku apps") <>
        command "increment"  (parseIncrement  `withInfo` "Increment a app's visit count")

parseSearch :: Parser Command
parseSearch = Search <$> optional (argument str (metavar "QUERY"))

parseCache :: Parser Command
parseCache = pure Cache

parseIncrement :: Parser Command
parseIncrement = Increment <$> argument str (metavar "APP_URL")

cacheFile :: String
cacheFile = "apps.csv"
