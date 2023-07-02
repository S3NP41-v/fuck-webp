{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.ByteString.Lazy as B
import Data.Text            ( Text, splitOn, pack, unpack )
import Data.List            ( find )
import Data.Aeson           ( (.:), object, FromJSON(parseJSON), Value(Object), KeyValue((.=)), ToJSON(toJSON), decode )
import System.Directory     ( listDirectory, setCurrentDirectory, getPermissions, Permissions (readable) )
import System.Process       ( callCommand )
import Control.Concurrent   ( threadDelay )


main :: IO ()
main = do
    mConf <- getConfig
    case mConf of
        Nothing     -> putStrLn "Could not read Config"
        Just config -> scanAndReplace (scanPaths config) (convertFromTo config) (scanFrequency config)


scanAndReplace :: [FilePath] -> [(Text, Text)] -> Int -> IO ()
scanAndReplace paths fromTo delay = do
    threadDelay (delay * 10^6)
    
    mapM_ (\path -> do
        files <- listDirectory path
        mapM_ (\file -> do
            setCurrentDirectory path
            let nameAndExt = splitOn "." (pack file)

            case find (\(from, to) -> from == last nameAndExt) fromTo of
                Nothing         -> return ()
                Just (from, to) -> do callCommand $ "ffmpeg -hide_banner -loglevel error -y -i \"" <> file <> "\" \"" <> unpack (head nameAndExt) <> "." <> unpack to <> "\"" <> " && del \"" <> file <> "\""
            ) files
        ) paths
    scanAndReplace paths fromTo delay


getConfig :: IO (Maybe Config)
getConfig = do
    file <- B.readFile "config.json"
    return $ decode file 


data Config = Config 
    { scanPaths     :: [FilePath]
    , convertFromTo :: [(Text, Text)]
    , scanFrequency :: Int
    }

instance FromJSON Config where
    parseJSON (Object v) = 
        Config <$> v .: "scanPaths"
               <*> v .: "convertFromTo"
               <*> v .: "scanFrequency"

instance ToJSON Config where
    toJSON (Config scanPaths convertFromTo scanFrequency) = 
        object  [ "scanPaths"       .= scanPaths
                , "convertFromTo"   .= convertFromTo
                , "scanFrequency"   .= scanFrequency
                ]
