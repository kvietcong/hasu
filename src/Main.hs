{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Maybe
import Network.HTTP.Simple
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

data Post = Post { uid :: Int
                 , id :: Int
                 , title :: String
                 , body :: String
                 } deriving Show

instance FromJSON Post where
    parseJSON = withObject "Post" $ \o -> Post
        <$> o .: "userId"
        <*> o .: "id"
        <*> o .: "title"
        <*> o .: "body"

postsEndpoint :: Request
postsEndpoint = "https://jsonplaceholder.typicode.com/posts"

getPosts :: Response LB.ByteString -> Maybe [Post]
getPosts = decode . getResponseBody

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
    , scoreID      :: Maybe String }
    deriving Show

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

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getRecentPlays :: Response LB.ByteString -> Maybe [RecentPlay]
getRecentPlays = decode . getResponseBody

recentPlaysRequest :: Query -> Request
recentPlaysRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                           $ setRequestPath "/api/get_user_recent"
                           $ setRequestQueryString options
                           osuDomain

main :: IO ()
main = do
    postsResponse <- httpLBS postsEndpoint
    let posts = fromMaybe [] (getPosts postsResponse)
    print $ take 2 posts

    putStrLn "Please input your osu! API key"
    osuAPIKey <- BU.fromString <$> getLine

    putStrLn $ "Your API Key: " ++ show osuAPIKey

    let options = [ ("k", Just osuAPIKey)
                  , ("u", Just "7358268")
                  , ("m", Just "0")
                  , ("type", Just "id")
                  , ("event_days", Just "31")]

    recentPlaysResponse <- httpLBS $ recentPlaysRequest options
    
    print $ fromMaybe [] (getRecentPlays recentPlaysResponse)
    pure ()
