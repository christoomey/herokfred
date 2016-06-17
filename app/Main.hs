module Main where

import Options.Applicative

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Interact with Heroku API")


data Command
    = Search (Maybe String)
    | Cache


run :: Command -> IO ()
run cmd =
    case cmd of
        Search (Just query) -> putStrLn $ "Search " <> query
        Search (Nothing) -> putStrLn "Search, but no query!!!"
        Cache         -> putStrLn "Cache"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
    subparser $
        command "search" (parseSearch `withInfo` "Search for Heroku apps") <>
        command "cache" (parseCache  `withInfo` "Cache Heroku apps")

parseSearch :: Parser Command
parseSearch = Search <$> optional (argument str (metavar "QUERY"))

parseCache :: Parser Command
parseCache = pure Cache

cacheFile :: String
cacheFile = "apps.csv"
