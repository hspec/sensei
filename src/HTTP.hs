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
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Network.Socket

socketName :: FilePath -> String
socketName dir = dir </> ".sensei.sock"

socketAddr :: FilePath -> SockAddr
socketAddr = SockAddrUnix . socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket newSocket close

withServer :: FilePath -> IO (Bool, String) -> IO a -> IO a
withServer dir trigger = withApplication dir (app trigger)

withApplication :: FilePath -> Application -> IO a -> IO a
withApplication dir application action = do
  removeSocketFile dir
  withSocket $ \ sock -> do
    bracket_ (bind sock $ socketAddr dir) (removeSocketFile dir) $ do
      listen sock maxListenQueue
      withThread (runSettingsSocket defaultSettings sock application) action

removeSocketFile :: FilePath -> IO ()
removeSocketFile dir = void $ tryJust (guard . isDoesNotExistError) (removeFile $ socketName dir)

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
