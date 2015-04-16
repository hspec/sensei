{-# LANGUAGE OverloadedStrings #-}
module Http (start) where

import           Data.String
import           Control.Monad
import           Control.Concurrent
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)

start :: IO String -> IO ()
start trigger = do
  putStrLn $ "listening on http://localhost:" ++ show port
  void . forkIO $ run port (app trigger)
  where
    port = 8080

app :: IO String -> Application
app trigger _ respond = trigger >>= textPlain . fromString
  where
    textPlain = respond . responseLBS status200 [(hContentType, "text/plain")]
