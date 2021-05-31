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
    { scoreUserID           :: Maybe Int
    , scoreUsername         :: Maybe String
    , scoreBeatmapID        :: Maybe Int
    , scoreScoreID          :: Maybe String
    , scoreScore            :: Int
    , scoreMaxCombo         :: Int
    , scoreCount50          :: Int
    , scoreCount100         :: Int
    , scoreCount300         :: Int
    , scoreCountMiss        :: Int
    , scoreCountKatu        :: Int
    , scoreCountGeki        :: Int
    , scorePerfect          :: Int
    , scoreEnabledMods      :: Maybe Int
    , scoreDate             :: String
    , scoreRank             :: String
    , scorePp               :: Maybe Double
    , scoreReplayAvailable  :: Maybe Bool
    , scoreSlot             :: Maybe Int
    , scoreTeam             :: Maybe Int
    , scorePass             :: Maybe Bool
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
