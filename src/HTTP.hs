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
import           Data.Aeson (ToJSON(..), encode)
import           Data.ByteString.Builder
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.Wai
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Status as Status
import           Network.HTTP.Media
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Network.Socket

import qualified Trigger
import           GHC.Diagnostic

socketName :: FilePath -> String
socketName dir = dir </> ".sensei.sock"

socketAddr :: FilePath -> SockAddr
socketAddr = SockAddrUnix . socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket newSocket close

withServer :: FilePath -> IO (Trigger.Result, String, [Diagnostic]) -> IO a -> IO a
withServer dir = withApplication dir . app

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

app :: IO (Trigger.Result, String, [Diagnostic]) -> Application
app getLastResult request respond = case pathInfo request of

  [] -> requireMethod "GET" $ do
    getLastResult >>= textPlain

  ["diagnostics"] -> requireMethod "GET" $ do
    (_, _, diagnostics) <- getLastResult
    respond $ json diagnostics

  _ -> do
    respond $ genericStatus Status.notFound404 request

  where
    color :: Either Builder Bool
    color = case join $ lookup "color" $ queryString request of
      Nothing -> Right True
      Just "false" -> Right False
      Just "true" -> Right True
      Just value -> Left $ "invalid value for color: " <> urlEncodeBuilder True value

    textPlain :: (Trigger.Result, FilePath, [Diagnostic]) -> IO ResponseReceived
    textPlain (result, xs, _diagnostics) = case color of
      Left err -> respond $ responseBuilder Status.badRequest400 [(hContentType, "text/plain")] err
      Right c -> respond $ responseLBS status [(hContentType, "text/plain")] (encodeUtf8 . fromString $ strip xs)
        where
          strip :: String -> String
          strip
            | c = id
            | otherwise = stripAnsi

          status :: Status
          status = case result of
            Trigger.HookFailed -> Status.internalServerError500
            Trigger.Failure -> Status.internalServerError500
            Trigger.Success -> Status.ok200

    requireMethod :: Method -> IO ResponseReceived -> IO ResponseReceived
    requireMethod required action = case requestMethod request of
      method | method == required -> action
      _ -> respond $ genericRfc7807Response Status.methodNotAllowed405

    json :: ToJSON a => a -> Response
    json = responseLBS Status.ok200 [(hContentType, "application/json")] . encode

genericStatus :: Status -> Request -> Response
genericStatus status@(Status number message) request = fromMaybe text $ mapAcceptMedia [
    ("text/plain", text)
  , ("application/json", json)
  ] =<< lookup "Accept" request.requestHeaders
  where
    text :: Response
    text = responseBuilder
      status
      [(hContentType, "text/plain")]
      body
      where
        body :: Builder
        body = intDec number <> " " <> byteString message

    json :: Response
    json = genericRfc7807Response status

genericRfc7807Response :: Status -> Response
genericRfc7807Response status@(Status number message) = responseBuilder
  status
  [(hContentType, "application/json")]
  body
  where
    body :: Builder
    body = "{\"title\":\"" <> byteString message <> "\",\"status\":" <> intDec number <> "}"

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
