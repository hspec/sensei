{-# LANGUAGE OverloadedStrings #-}
module HTTP (
  withServer
, socketName
, newSocket
, socketAddr

-- exported for testing
, app
) where

import           Prelude ()
import           Prelude.Compat
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           System.IO.Error
import           System.Directory
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Network.Socket

socketName :: String
socketName = ".autospec.sock"

socketAddr :: SockAddr
socketAddr = SockAddrUnix socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket action = bracket newSocket sClose action

withServer :: IO (Bool, String) -> IO a -> IO a
withServer trigger = withApplication (app trigger)

withApplication :: Application -> IO a -> IO a
withApplication application action = do
  _ <- tryJust (guard . isDoesNotExistError) (removeFile socketName)
  withSocket $ \sock -> do
    bind sock socketAddr
    listen sock maxListenQueue
    withThread (runSettingsSocket defaultSettings sock application) action

withThread :: IO () -> IO a -> IO a
withThread asyncAction action = do
  mvar <- newEmptyMVar
  tid <- forkIO $ do
    asyncAction `finally` putMVar mvar ()
  r <- action
  killThread tid
  takeMVar mvar
  return r

app :: IO (Bool, String) -> Application
app trigger _ respond = trigger >>= textPlain
  where
    textPlain (success, xs) = respond $ responseLBS status [(hContentType, "text/plain")] (encodeUtf8 . fromString $ xs)
      where
        status
          | success = ok200
          | otherwise = internalServerError500
