{-# LANGUAGE OverloadedStrings #-}
import Score
import Beatmap
import HasuBase

import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (httpLBS)
import qualified Data.ByteString.UTF8 as BU (fromString)

main :: IO ()
main = do
    putStrLn "Please input your osu! API key"
    auth <- getAuthentication . BU.fromString <$> getLine
    putStrLn ""

    recentPlaysResponse <- httpLBS
                         $ recentPlaysRequest
                         $ configureRequest auth [ ("u", "7358268")
                                                 , ("limit", "5")
                                                 , ("type", "id") ]
    putStrLn "Recent Plays"
    mapM_ print (fromMaybe [] (getScores recentPlaysResponse))
    putStrLn ""

    getBeatmapResponse <- httpLBS
                         $ getBeatmapsRequest
                         $ configureRequest auth [("b", "2932984")]
    putStrLn "Beatmap"
    print $ head (fromMaybe [] (getBeatmaps getBeatmapResponse))
