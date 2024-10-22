{-# LANGUAGE CPP #-}
module HTTP (
  withServer
, socketName
, newSocket
, socketAddr

#ifdef TEST
, app
, stripAnsi
#endif
) where

import           Imports hiding (strip, encodeUtf8)

import           System.Directory
import qualified Data.ByteString.Lazy as L
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
app trigger request respond = trigger >>= textPlain
  where
    color :: Either ByteString Bool
    color = case join $ lookup "color" $ queryString request of
      Nothing -> Right True
      Just "false" -> Right False
      Just "true" -> Right True
      Just value -> Left $ "invalid value for color: " <> urlEncode True value

    textPlain :: (Trigger.Result, FilePath) -> IO ResponseReceived
    textPlain (result, xs) = case color of
      Left err -> respond $ responseLBS status400 [(hContentType, "text/plain")] (L.fromStrict err)
      Right c -> respond $ responseLBS status [(hContentType, "text/plain")] (encodeUtf8 . fromString $ strip xs)
        where
          strip :: String -> String
          strip
            | c = id
            | otherwise = stripAnsi

          status = case result of
            Trigger.HookFailed -> status500
            Trigger.Failure -> status500
            Trigger.Success -> status200

-- |
-- Remove terminal sequences.
stripAnsi :: String -> String
stripAnsi = go
  where
    go input = case input of
      '\ESC' : '[' :       (dropNumericParameters -> c : xs) | isCommand c -> go xs
      '\ESC' : '[' : '?' : (dropNumericParameters -> c : xs) | isCommand c -> go xs
      x : xs -> x : go xs
      [] -> []

    dropNumericParameters :: FilePath -> FilePath
    dropNumericParameters = dropWhile (`elem` ("0123456789;" :: [Char]))

    isCommand :: Char -> Bool
    isCommand = (`elem` commands)

    commands :: FilePath
    commands = ['A'..'Z'] <> ['a'..'z']
