{-# LANGUAGE OverloadedStrings #-}
module User
    ( Event(..)
    , User(..)
    , userRequest
    , getUsers
    ) where

import HasuBase
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LB

data Event = Event
    { displayHTML       :: String
    , beatmapID         :: Int
    , beatmapSetID      :: Int
    , date              :: String
    , epicFactor        :: Int
    } deriving Show

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> Event
        <$> o .: "display_html"
        <*> (read <$> o .: "beatmap_id")
        <*> (read <$> o .: "beatmapset_id")
        <*> o .: "date"
        <*> (read <$> o .: "epicfactor")

data User = User
    { id                :: Int
    , name              :: String
    , joinDate          :: String
    , count300          :: Int
    , count100          :: Int
    , count50           :: Int
    , playcount         :: Int
    , rankedScore       :: Int
    , totalScore        :: Int
    , ppRank            :: Int
    , level             :: Double
    , ppRaw             :: Double
    , accuracy          :: Double
    , countRankSS       :: Int
    , countRankSSH      :: Int
    , countRankS        :: Int
    , countRankSH       :: Int
    , countRankA        :: Int
    , country           :: String
    , secondsPlayed     :: Int
    , ppCountryRank     :: Int
    , events            :: [Event]
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

getUsers :: Response LB.ByteString -> Maybe [User]
getUsers = decode . getResponseBody

userRequest :: Query -> Request
userRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                      $ setRequestPath "/api/get_user"
                      $ setRequestQueryString options
                      osuDomain
