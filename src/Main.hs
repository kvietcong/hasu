{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Maybe
import Network.HTTP.Simple
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

data RecentPlay = RecentPlay
    { beatmapID    :: Int
    , score        :: Int
    , maxCombo     :: Int
    , count50      :: Int
    , count100     :: Int
    , count300     :: Int
    , countMiss    :: Int
    , countKatu    :: Int
    , countGeki    :: Int
    , perfect      :: Int
    , enabledMods  :: Int
    , userID       :: Int
    , date         :: String
    , rank         :: String
    , scoreID      :: Maybe String } deriving Show

instance FromJSON RecentPlay where
    parseJSON = withObject "RecentPlay" $ \o -> RecentPlay
        <$> (read <$> o .: "beatmap_id")
        <*> (read <$> o .: "score")
        <*> (read <$> o .: "maxcombo")
        <*> (read <$> o .: "count50")
        <*> (read <$> o .: "count100")
        <*> (read <$> o .: "count300")
        <*> (read <$> o .: "countmiss")
        <*> (read <$> o .: "countkatu")
        <*> (read <$> o .: "countgeki")
        <*> (read <$> o .: "perfect")
        <*> (read <$> o .: "enabled_mods")
        <*> (read <$> o .: "user_id")
        <*> o .: "date"
        <*> o .: "rank"
        <*> optional (o .: "score_id")

newtype Authentication = Authentication Query

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getRecentPlays :: Response LB.ByteString -> Maybe [RecentPlay]
getRecentPlays = decode . getResponseBody

recentPlaysRequest :: Query -> Request
recentPlaysRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                           $ setRequestPath "/api/get_user_recent"
                           $ setRequestQueryString options
                           osuDomain

getAuthentication :: BU.ByteString -> Authentication
getAuthentication apiKey = Authentication [("k", Just apiKey)]

configureRecentPlaysRequest :: Authentication ->
    BU.ByteString -> BU.ByteString -> BU.ByteString -> BU.ByteString -> Query
configureRecentPlaysRequest
    (Authentication query) u m limit type' = [ ("u", Just u)
                                             , ("m", Just m)
                                             , ("limit", Just limit)
                                             , ("type", Just type') ] ++ query

main :: IO ()
main = do
    putStrLn "Please input your osu! API key"
    auth <- getAuthentication . BU.fromString <$> getLine

    recentPlaysResponse <- httpLBS
                         $ recentPlaysRequest
                         $ configureRecentPlaysRequest auth "7358268" "0" "50" "id"
    
    mapM_ print (fromMaybe [] (getRecentPlays recentPlaysResponse))
