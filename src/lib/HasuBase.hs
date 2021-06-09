{-# LANGUAGE OverloadedStrings #-}
module HasuBase
    ( Authentication(..)
    , osuDomain
    , getAuthentication
    , getAuthenticationQuery
    , configureRequest
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BU

newtype Authentication = Authentication Query

osuDomain :: Request
osuDomain = "https://osu.ppy.sh"

getAuthentication :: BU.ByteString -> Authentication
getAuthentication apiKey = Authentication [("k", Just apiKey)]

getAuthenticationQuery :: Authentication -> Query
getAuthenticationQuery (Authentication q) = q

configureRequest :: Authentication -> [(BU.ByteString, BU.ByteString)] -> Query
configureRequest (Authentication query) options =
    [(option, Just value) | (option, value) <- options] ++ query
