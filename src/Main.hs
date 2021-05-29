{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Maybe
import Data.Aeson.Types
import Network.HTTP.Simple
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

data Event = Event
    { eventDisplayHTML      :: String
    , eventBeatmapID        :: Int
    , eventBeatmapSetID     :: Int
    , eventDate             :: String
    , eventEpicFactor       :: Int
    } deriving Show

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> Event
        <$> o .: "display_html"
        <*> (read <$> o .: "beatmap_id")
        <*> (read <$> o .: "beatmapset_id")
        <*> o .: "date"
        <*> (read <$> o .: "epicfactor")

data User = User
    { userID                :: Int
    , username              :: String
    , userJoinDate          :: String
    , userCount300          :: Int
    , userCount100          :: Int
    , userCount50           :: Int
    , userPlaycount         :: Int
    , userRankedScore       :: Int
    , userTotalScore        :: Int
    , userPPRank            :: Int
    , userLevel             :: Double
    , userPPRaw             :: Double
    , userAccuracy          :: Double
    , userCountRankSS       :: Int
    , userCountRankSSH      :: Int
    , userCountRankS        :: Int
    , userCountRankSH       :: Int
    , userCountRankA        :: Int
    , userCountry           :: String
    , userSecondsPlayed     :: Int
    , userPPCountryRank     :: Int
    , events                :: [Event]
    } deriving Show

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> (read <$> o .: "user_id")
        <*> o .: "username"
        <*> o .: "join_date"
        <*> (read <$> o .: "count300")
        <*> (read <$> o .: "count100")
        <*> (read <$> o .: "count50")
        <*> (read <$> o .: "playcount")
        <*> (read <$> o .: "ranked_score")
        <*> (read <$> o .: "total_score")
        <*> (read <$> o .: "pp_rank")
        <*> (read <$> o .: "level")
        <*> (read <$> o .: "pp_raw")
        <*> (read <$> o .: "accuracy")
        <*> (read <$> o .: "count_rank_ss")
        <*> (read <$> o .: "count_rank_ssh")
        <*> (read <$> o .: "count_rank_s")
        <*> (read <$> o .: "count_rank_sh")
        <*> (read <$> o .: "count_rank_a")
        <*> o .: "country"
        <*> (read <$> o .: "total_seconds_played")
        <*> (read <$> o .: "pp_country_rank")
        <*> o .: "events"

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

newtype Authentication = Authentication Query

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getAuthentication :: BU.ByteString -> Authentication
getAuthentication apiKey = Authentication [("k", Just apiKey)]

getScores :: Response LB.ByteString -> Maybe [Score]
getScores = decode . getResponseBody

getUsers :: Response LB.ByteString -> Maybe [User]
getUsers = decode . getResponseBody

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

userRequest :: Query -> Request
userRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                      $ setRequestPath "/api/get_user"
                      $ setRequestQueryString options
                      osuDomain

configureUserRequest :: Authentication ->
    BU.ByteString -> BU.ByteString -> BU.ByteString -> BU.ByteString -> Query
configureUserRequest (Authentication query) u m type' event_days =
    [ ("u", Just u)
    , ("m", Just m)
    , ("type", Just type')
    , ("event_days", Just event_days) ] ++ query

main :: IO ()
main = do
    putStrLn "Please input your osu! API key"
    auth <- getAuthentication . BU.fromString <$> getLine

    recentPlaysResponse <- httpLBS
                         $ recentPlaysRequest
                         $ configureRecentPlaysRequest auth "7358268" "0" "50" "id"
    
    putStrLn "Recent"
    mapM_ print (fromMaybe [] (getScores recentPlaysResponse))

    scoresResponse <- httpLBS
                    $ scoresRequest
                    $ configureScoresRequest auth "1655981" "Vel0ciTy" "" "" "string" ""
    
    putStrLn "Scores"
    mapM_ print (fromMaybe [] (getScores scoresResponse))

    userBestResponse <- httpLBS
                      $ userBestRequest
                      $ configureUserBestRequest auth "Vel0ciTy" "" "10" "string"
    
    putStrLn "Best"
    mapM_ print (fromMaybe [] (getScores userBestResponse))

    userResponse <- httpLBS
                  $ userRequest
                  $ configureUserRequest auth "Vel0ciTy" "0" "string" "31"
    
    putStrLn "User"
    case getUsers userResponse of
      Nothing -> putStrLn "No User :("
      Just a  -> print $ head a
