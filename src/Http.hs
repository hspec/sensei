{-# LANGUAGE OverloadedStrings #-}
module Http (start) where

import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
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
app trigger _ respond = trigger >>= textPlain . encodeUtf8 . fromString
  where
    textPlain = respond . responseLBS status200 [(hContentType, "text/plain")]
