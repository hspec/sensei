{-# LANGUAGE OverloadedStrings #-}
module Http (
  start

-- exported for testing
, app
) where

import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Control.Exception
import           Control.Monad
import           System.IO.Error
import           System.Directory
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Network.Socket

start :: String -> IO (Bool, String) -> IO ()
start socketName trigger = do
  _ <- tryJust (guard . isDoesNotExistError) (removeFile socketName)
  bracket (socket AF_UNIX Stream 0) close $ \sock -> do
    bind sock (SockAddrUnix socketName)
    listen sock maxListenQueue
    runSettingsSocket defaultSettings sock (app trigger)

app :: IO (Bool, String) -> Application
app trigger _ respond = trigger >>= textPlain
  where
    textPlain (success, xs) = respond $ responseLBS status [(hContentType, "text/plain")] (encodeUtf8 . fromString $ xs)
      where
        status
          | success = ok200
          | otherwise = preconditionFailed412
