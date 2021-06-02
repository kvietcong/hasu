{-# LANGUAGE OverloadedStrings #-}
module Score
    ( Score(..)
    , scoresRequest
    , configureScoresRequest
    , recentPlaysRequest
    , configureRecentPlaysRequest
    , userBestRequest
    , configureUserBestRequest
    , getScores
    ) where

import HasuBase
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

data Score = Score
    { userID           :: Maybe Int
    , username         :: Maybe String
    , beatmapID        :: Maybe Int
    , scoreID          :: Maybe String
    , score            :: Int
    , maxCombo         :: Int
    , count50          :: Int
    , count100         :: Int
    , count300         :: Int
    , countMiss        :: Int
    , countKatu        :: Int
    , countGeki        :: Int
    , perfect          :: Int
    , enabledMods      :: Maybe Int
    , date             :: String
    , rank             :: String
    , pp               :: Maybe Double
    , replayAvailable  :: Maybe Bool
    , slot             :: Maybe Int
    , team             :: Maybe Int
    , pass             :: Maybe Bool
    } deriving Show

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
        <*> optional (read <$> o .: "enabled_mods")
        <*> o .: "date"
        <*> o .: "rank"
        <*> optional (read <$> o .: "pp")
        <*> optional ((=="1") <$> (o .: "replay_available" :: Parser String))
        <*> optional (read <$> o .: "slot")
        <*> optional (read <$> o .: "team")
        <*> optional ((=="1") <$> (o .: "pass" :: Parser String))


getScores :: Response LB.ByteString -> Maybe [Score]
getScores = decode . getResponseBody

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
