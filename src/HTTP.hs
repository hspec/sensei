{-# LANGUAGE CPP #-}
module HTTP (
  AppConfig(..)
, withApp

#ifdef TEST
, app
#endif
) where

import           Imports hiding (strip, encodeUtf8)

import           System.Directory
import           Data.Aeson
import           Data.ByteString.Builder
import           Network.Wai
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Status as Status
import           Network.HTTP.Media
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Network.Socket

import           Util
import qualified Trigger
import qualified Config.DeepSeek as Config
import qualified DeepSeek
import           GHC.Diagnostic hiding (edits)
import qualified GHC.Diagnostic as Diagnostic

import           HTTP.Util
import           Sensei.API (QuickFixRequest(..), DeepFixRequest(..))
import qualified Sensei.API as API

data AppConfig = AppConfig {
  dir :: FilePath
, hieDir :: FilePath
, putStrLn :: String -> IO ()
, deepSeek :: Maybe Config.DeepSeek
, trigger :: IO ()
, getLastResult :: IO (Trigger.Result, String, [Annotated])
, getModules :: IO [String]
}

socketAddr :: FilePath -> SockAddr
socketAddr = SockAddrUnix . socketName

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket newSocket close

withApp :: AppConfig -> IO a -> IO a
withApp config = withApplication config.dir $ app config

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

app :: AppConfig -> Application
app config@AppConfig { putStrLn, dir, getLastResult } request respond = case pathInfo request of

  [] -> requireMethod "GET" $ do
    getLastResult >>= textPlain

  ["diagnostics"] -> requireMethod "GET" $ do
    getDiagnostics >>= respond . json . map (.diagnostic)

  ["modules"] -> requireMethod "GET" $ do
    config.getModules >>= respond . json . API.Modules

  ["config"] -> requireMethod "GET" do
    respond $ json API.Config { hieDir = config.hieDir }

  ["trigger"] -> requireMethod "POST" do
    config.trigger
    noContent

  ["quick-fix"] -> requireMethod "POST" $ do
    withJsonBody @QuickFixRequest \ quickFixRequest -> do
      edits <- concatMap Diagnostic.edits <$> getDiagnostics
      apply dir quickFixRequest.choice edits
      noContent

  ["deep-fix"] -> requireMethod "POST" $ do
    withJsonBody @DeepFixRequest \ deepFixRequest -> do
      diagnostics <- map (.diagnostic) <$> getDiagnostics
      case theseFromMaybes (head diagnostics) deepFixRequest.instructions of
        Just instructions -> withDeepSeekConfig \ conf -> do
          DeepSeek.apply putStrLn conf dir instructions
          noContent
        Nothing -> do
          noContent

  _ -> do
    respond $ genericStatus Status.notFound404 request

  where
    withJsonBody :: FromJSON a => (a -> IO ResponseReceived) -> IO ResponseReceived
    withJsonBody action = consumeRequestBodyLazy request <&> eitherDecode >>= \ case
      Right quickFixRequest -> action quickFixRequest
      Left err -> badRequest err

    getDiagnostics :: IO [Annotated]
    getDiagnostics = do
      (_, _, diagnostics) <- getLastResult
      return diagnostics

    withDeepSeekConfig :: (Config.DeepSeek -> IO ResponseReceived) -> IO ResponseReceived
    withDeepSeekConfig action = case config.deepSeek of
      Just conf -> action conf
      Nothing -> serviceUnavailable "missing config value deep-seek.auth"

    noContent :: IO ResponseReceived
    noContent = respond $ jsonResponse Status.noContent204 ""

    serviceUnavailable :: String -> IO ResponseReceived
    serviceUnavailable = respond . genericRfc7807Response Status.serviceUnavailable503 . Just

    badRequest :: String -> IO ResponseReceived
    badRequest = respond . genericRfc7807Response Status.badRequest400 . Just

    color :: Either Builder Bool
    color = case join $ lookup "color" $ queryString request of
      Nothing -> Right True
      Just "false" -> Right False
      Just "true" -> Right True
      Just value -> Left $ "invalid value for color: " <> urlEncodeBuilder True value

    textPlain :: (Trigger.Result, FilePath, [Annotated]) -> IO ResponseReceived
    textPlain (result, xs, _diagnostics) = case color of
      Left err -> respond $ textResponse Status.badRequest400 err
      Right c -> respond . textResponse status . stringUtf8 $ strip xs
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
      _ -> respond $ genericRfc7807Response Status.methodNotAllowed405 Nothing

    json :: ToJSON a => a -> Response
    json = jsonResponse Status.ok200 . fromEncoding . toEncoding

genericStatus :: Status -> Request -> Response
genericStatus status@(Status number message) request = fromMaybe text $ mapAcceptMedia [
    ("text/plain", text)
  , ("application/json", json)
  ] =<< lookup "Accept" request.requestHeaders
  where
    text :: Response
    text = textResponse status $ intDec number <> " " <> byteString message

    json :: Response
    json = genericRfc7807Response status Nothing

genericRfc7807Response :: Status -> Maybe String -> Response
genericRfc7807Response status@(Status number message) detail = jsonResponse status body
  where
    body :: Builder
    body = "{\n  \"title\": \"" <> byteString message <> "\",\n  \"status\": " <> intDec number <> renderedDetail <> "\n}"

    renderedDetail :: Builder
    renderedDetail = case detail of
      Nothing -> ""
      Just err -> ",\n  \"detail\": " <> lazyByteString (encode err)

jsonResponse :: Status -> Builder -> Response
jsonResponse status body = responseBuilder status [(hContentType, "application/json")] body

textResponse :: Status -> Builder -> Response
textResponse status = responseBuilder status [(hContentType, "text/plain")]
