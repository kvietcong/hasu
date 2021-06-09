{-# LANGUAGE OverloadedStrings #-}
module Beatmap
    (Beatmap(..)
    , getBeatmaps
    , getBeatmap
    , getBeatmapsRequest
    )
where

import HasuBase
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LB

data Beatmap = Beatmap
    { approved          :: Int
    , submitDate        :: String
    , approvedDate      :: Maybe String
    , lastUpdate        :: String
    , artist            :: String
    , beatmapID         :: Int
    , beatmapSetID      :: Int
    , bpm               :: Int
    , creator           :: String
    , creatorID         :: Int
    , difficultyRating  :: Double
    , diffAim           :: Double
    , diffSpeed         :: Double
    , diffSize          :: Double
    , diffOverall       :: Double
    , diffApproach      :: Double
    , diffDrain         :: Double
    , hitLength         :: Int
    , source            :: String
    , genreID           :: Int
    , languageID        :: Int
    , packs             :: Maybe String
    , title             :: String
    , totalLength       :: Int
    , version           :: String
    , fileMD5           :: String
    , mode              :: Int
    , tags              :: [String]
    , favoriteCount     :: Int
    , rating            :: Double
    , playCount         :: Int
    , passCount         :: Int
    , countNormal       :: Int
    , countSlider       :: Int
    , countSpinner      :: Int
    , maxCombo          :: Int
    , storyBoard        :: Bool
    , video             :: Bool
    , downloadAvailable :: Bool
    , audioAvailable    :: Bool
    } deriving Show 

instance FromJSON Beatmap where
    parseJSON = withObject "Beatmap" $ \o -> Beatmap
        <$> (read <$> o .: "approved")
        <*> (o .: "submit_date")
        <*> optional (o .: "approved_date")
        <*> (o .: "last_update")
        <*> (o .: "artist")
        <*> (read <$> o .: "beatmap_id")
        <*> (read <$> o .: "beatmapset_id")
        <*> (read <$> o .: "bpm")
        <*> (o .: "creator")
        <*> (read <$> o .: "creator_id")
        <*> (read <$> o .: "difficultyrating")
        <*> (read <$> o .: "diff_aim")
        <*> (read <$> o .: "diff_speed")
        <*> (read <$> o .: "diff_size")
        <*> (read <$> o .: "diff_overall")
        <*> (read <$> o .: "diff_approach")
        <*> (read <$> o .: "diff_drain")
        <*> (read <$> o .: "hit_length")
        <*> (o .: "source")
        <*> (read <$> o .: "genre_id")
        <*> (read <$> o .: "language_id")
        <*> (o .: "packs")
        <*> (o .: "title")
        <*> (read <$> o .: "total_length")
        <*> (o .: "version")
        <*> (o .: "file_md5")
        <*> (read <$> o .: "mode")
        <*> (words <$> o .: "tags")
        <*> (read <$> o .: "favourite_count")
        <*> (read <$> o .: "rating")
        <*> (read <$> o .: "playcount")
        <*> (read <$> o .: "passcount")
        <*> (read <$> o .: "count_normal")
        <*> (read <$> o .: "count_slider")
        <*> (read <$> o .: "count_spinner")
        <*> (read <$> o .: "max_combo")
        <*> ((=="1") <$> (o .: "storyboard" :: Parser String))
        <*> ((=="1") <$> (o .: "video" :: Parser String))
        <*> ((=="0") <$> (o .: "download_unavailable" :: Parser String))
        <*> ((=="0") <$> (o .: "audio_unavailable" :: Parser String))


getBeatmaps :: Response LB.ByteString -> Maybe [Beatmap]
getBeatmaps = decode . getResponseBody

getBeatmap :: Response LB.ByteString -> Maybe Beatmap
getBeatmap = decode . getResponseBody

getBeatmapsRequest :: Query -> Request
getBeatmapsRequest options = setRequestBodyLBS "SOMEBODY ONCE TOLD ME"
                      $ setRequestPath "/api/get_beatmaps"
                      $ setRequestQueryString options
                      osuDomain
