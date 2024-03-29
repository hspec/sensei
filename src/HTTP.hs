{-# LANGUAGE CPP #-}
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

import qualified Trigger

socketName :: FilePath -> String
socketName dir = dir </> ".sensei.sock"

socketAddr :: FilePath -> SockAddr
socketAddr = SockAddrUnix . socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket newSocket close

withServer :: FilePath -> IO (Trigger.Result, String) -> IO a -> IO a
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

app :: IO (Trigger.Result, String) -> Application
app trigger _ respond = trigger >>= textPlain
  where
    textPlain (result, xs) = respond $ responseLBS status [(hContentType, "text/plain")] (encodeUtf8 . fromString $ xs)
      where
        status = case result of
          Trigger.HookFailed -> internalServerError500
          Trigger.Failure -> internalServerError500
          Trigger.Success -> ok200
