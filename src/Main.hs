{-# LANGUAGE OverloadedStrings #-}
import Score
import Beatmap
import HasuBase

import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (httpLBS, getResponseBody)
import qualified Data.ByteString.UTF8 as BU (fromString)

main :: IO ()
main = do
    putStrLn "Please input your osu! API key"
    auth <- getAuthentication . BU.fromString <$> getLine

    recentPlaysResponse <- httpLBS
                         $ recentPlaysRequest
                         $ configureRecentPlaysRequest auth "7358268" "0" "5" "id"
    
    putStrLn "Recent"
    mapM_ print (fromMaybe [] (getScores recentPlaysResponse))

    getBeatmapsResponse <- httpLBS
                         $ getBeatmapsRequest
                         $ ("b", Just "2932984") : getAuthenticationQuery auth

    getBeatmapsResponse2 <- httpLBS
                          $ getBeatmapsRequest
                          $ configureBeatmapsRequest auth "" "2932984" "" "" "" "" "" "" "" ""
    
    putStrLn "Beatmaps"
    print $ getResponseBody getBeatmapsResponse
    mapM_ print (fromMaybe [] (getBeatmaps getBeatmapsResponse))
