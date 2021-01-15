{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module HTTP (
  withServer
, socketName
, newSocket
, socketAddr

#ifdef TEST
, app
#endif
) where

import           Imports

import           System.Directory
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Network.Socket

socketName :: String
socketName = ".sensei.sock"

socketAddr :: SockAddr
socketAddr = SockAddrUnix socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket action = bracket newSocket close action

withServer :: IO (Bool, String) -> IO a -> IO a
withServer trigger = withApplication (app trigger)

withApplication :: Application -> IO a -> IO a
withApplication application action = do
  removeSocketFile
  withSocket $ \sock -> do
    bracket_ (bind sock socketAddr) removeSocketFile $ do
      listen sock maxListenQueue
      withThread (runSettingsSocket defaultSettings sock application) action

removeSocketFile :: IO ()
removeSocketFile = void $ tryJust (guard . isDoesNotExistError) (removeFile socketName)

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
