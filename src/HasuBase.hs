{-# LANGUAGE OverloadedStrings #-}
module HasuBase
    ( Authentication(..)
    , osuDomain
    , getAuthentication
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BU

newtype Authentication = Authentication Query

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getAuthentication :: BU.ByteString -> Authentication
getAuthentication apiKey = Authentication [("k", Just apiKey)]

