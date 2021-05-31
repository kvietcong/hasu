{-# LANGUAGE OverloadedStrings #-}
import User
import Score
import HasuBase

import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (httpLBS)
import qualified Data.ByteString.UTF8 as BU (fromString)

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
