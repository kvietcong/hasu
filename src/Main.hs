{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Maybe
import Data.Aeson.Types
import Network.HTTP.Simple
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

data Score = Score
    { userID            :: Maybe Int
    , username          :: Maybe String
    , beatmapID         :: Maybe Int
    , scoreID           :: Maybe String
    , score             :: Int
    , maxCombo          :: Int
    , count50           :: Int
    , count100          :: Int
    , count300          :: Int
    , countMiss         :: Int
    , countKatu         :: Int
    , countGeki         :: Int
    , perfect           :: Int
    , enabledMods       :: Int
    , date              :: String
    , rank              :: String
    , pp                :: Maybe Double
    , replayAvailable   :: Maybe Bool } deriving Show

instance FromJSON Score where
    parseJSON = withObject "Score" $ \o -> Score
        <$> optional (read <$> o .: "user_id")
        <*> optional (o .: "username")
        <*> optional (read <$> o .: "beatmap_id")
        <*> optional (o .: "score_id")
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
        <*> o .: "date"
        <*> o .: "rank"
        <*> optional (read <$> o .: "pp")
        <*> optional ((=="1") <$> (o .: "replay_available" :: Parser String))

newtype Authentication = Authentication Query

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getAuthentication :: BU.ByteString -> Authentication
getAuthentication apiKey = Authentication [("k", Just apiKey)]

getScores :: Response LB.ByteString -> Maybe [Score]
getScores = decode . getResponseBody

recentPlaysRequest :: Query -> Request
recentPlaysRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                           $ setRequestPath "/api/get_user_recent"
                           $ setRequestQueryString options
                           osuDomain

configureRecentPlaysRequest :: Authentication ->
    BU.ByteString -> BU.ByteString -> BU.ByteString -> BU.ByteString -> Query
configureRecentPlaysRequest
    (Authentication query) u m limit type' = [ ("u", Just u)
                                             , ("m", Just m)
                                             , ("limit", Just limit)
                                             , ("type", Just type') ] ++ query

scoresRequest :: Query -> Request
scoresRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                      $ setRequestPath "/api/get_scores"
                      $ setRequestQueryString options
                      osuDomain

configureScoresRequest :: Authentication ->
    BU.ByteString -> BU.ByteString -> BU.ByteString -> BU.ByteString ->
    BU.ByteString -> BU.ByteString -> Query
configureScoresRequest (Authentication query) b u m mods type' limit =
    [ ("b", Just b)
    , ("u", Just u)
    , ("m", Just m)
    , ("mods", Just mods)
    , ("type", Just type')
    , ("limit", Just limit) ] ++ query

userBestRequest :: Query -> Request
userBestRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                      $ setRequestPath "/api/get_user_best"
                      $ setRequestQueryString options
                      osuDomain

configureUserBestRequest :: Authentication ->
    BU.ByteString -> BU.ByteString -> BU.ByteString -> BU.ByteString -> Query
configureUserBestRequest (Authentication query) u m limit type' =
    [ ("u", Just u)
    , ("m", Just m)
    , ("type", Just type')
    , ("limit", Just limit) ] ++ query

main :: IO ()
main = do
    putStrLn "Please input your osu! API key"
    auth <- getAuthentication . BU.fromString <$> getLine

    recentPlaysResponse <- httpLBS
                         $ recentPlaysRequest
                         $ configureRecentPlaysRequest auth "7358268" "0" "50" "id"
    
    mapM_ print (fromMaybe [] (getScores recentPlaysResponse))

    scoresResponse <- httpLBS
                    $ scoresRequest
                    $ configureScoresRequest auth "1655981" "Vel0ciTy" "" "" "string" ""
    
    print $ getResponseBody scoresResponse
    mapM_ print (fromMaybe [] (getScores scoresResponse))

    userBestResponse <- httpLBS
                      $ userBestRequest
                      $ configureUserBestRequest auth "Vel0ciTy" "" "100" "string"
    
    print $ getResponseBody scoresResponse
    mapM_ print (fromMaybe [] (getScores userBestResponse))
