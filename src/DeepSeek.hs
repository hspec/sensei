{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DeepSeek (
  apply
#ifdef TEST
, Patch(..)
, extractPatch
#endif
) where

import           Imports hiding (putStrLn, strip)

import           Data.Ord
import           System.Process
import           System.Environment.Blank
import           Data.ByteString.Lazy (toStrict)
import           Data.Aeson
import qualified Data.Yaml.Pretty as Yaml
import           Data.Text (Text)
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Simple

import           GHC.Diagnostic (Diagnostic)
import qualified GHC.Diagnostic.Type as Diagnostic
import qualified Config.DeepSeek as Config
import           DeepSeek.Types

separatorLength :: Int
separatorLength = 40

separator :: String
separator = take separatorLength $ repeat '#'

section :: String -> String
section name = take separatorLength $ take 3 separator ++ ' ' : name ++ ' ' : separator

apply :: (String -> IO ()) -> Config.DeepSeek -> FilePath -> Diagnostic -> IO ()
apply putStrLn config dir diagnostic = createChatCompletion dir diagnostic >>= \ case
  Nothing -> pass
  Just request -> (.choices) <$> query putStrLn config request >>= \ case
    [] -> pass
    choice : _ -> applyChoice choice
  where
    applyChoice :: Choice -> IO ()
    applyChoice choice = do
      case extractPatch diagnostic choice.message.content of
        Nothing -> putStrLn $ section "no patch"
        Just patch -> applyPatch putStrLn dir patch
      putStrLn separator

applyPatch :: (String -> IO ()) -> String -> Patch -> IO ()
applyPatch putStrLn dir patch = do
  env <- (:) ("POSIXLY_CORRECT", "true") <$> getEnvironment
  (status, out, err) <- readCreateProcessWithExitCode (proc "patch" ["-p" <> show patch.strip]) {
    cwd = guard (not $ null dir) $> dir
  , env = Just env
  } patch.diff
  when (status /= ExitSuccess) $ do
    putStrLn $ section "patch"
    unless (null out) $ putStrLn out
    unless (null err) $ putStrLn err

query :: (String -> IO ()) -> Config.DeepSeek -> CreateChatCompletion -> IO ChatCompletion
query putStrLn config (RequestBodyLBS . encode -> requestBody) = do
  body <- responseBody <$> httpLBS "https://api.deepseek.com/chat/completions" {
    method = "POST"
  , requestHeaders = [
      (hAccept, "application/json")
    , (hContentType, "application/json")
    , (hAuthorization, "Bearer " <> config.auth.bearer)
    ]
  , requestBody
  }
  logYamlBody body
  throwDecode body
  where
    logYamlBody :: LazyByteString -> IO ()
    logYamlBody body = do
      putStrLn $ section "response"
      putStrLn . decodeUtf8 $ toPrettyYaml body

    toPrettyYaml :: LazyByteString -> ByteString
    toPrettyYaml input = case decode @Value input of
      Nothing -> toStrict input
      Just value -> Yaml.encodePretty conf value
      where
        conf :: Yaml.Config
        conf = Yaml.setConfCompare (comparing fieldOrder) Yaml.defConfig

        fieldOrder :: Text -> Int
        fieldOrder name = fromMaybe maxBound . lookup name $ flip zip [1..] [
            "id"
          , "choices"
          , "created"
          , "model"
          , "system_fingerprint"
          , "object"
          , "usage"

          , "finish_reason"
          , "index"
          , "message"
          , "logprobs"

          , "content"
          , "role"

          , "completion_tokens"
          , "prompt_tokens"
          , "prompt_cache_hit_tokens"
          , "prompt_cache_miss_tokens"
          , "total_tokens"
          , "prompt_tokens_details"
          , "completion_tokens_details"
          ]

createChatCompletion :: FilePath -> Diagnostic -> IO (Maybe CreateChatCompletion)
createChatCompletion dir diagnostic = sequence $ diagnostic.span <&> \ span -> do
  source <- readFile (dir </> span.file)
  let
    content :: String
    content = unlines [
        "Given the following GHC diagnostics message, please suggest a fix for the corresponding Haskell code."
      , "Produce your fix as a unified diff so that it can be applied with the `patch` program."
      , "Don't provide explanations."
      , ""
      , "Enclose you answer in:"
      , ""
      , "```diff"
      , "--- " <> span.file
      , "+++ " <> span.file
      , "..."
      , "```"
      , ""
      , "(where ... is the placeholder for your answer)"
      , ""
      , "GHC diagnostics message:"
      , ""
      , "```console"
      , Diagnostic.format diagnostic
      , "```"
      , ""
      , "Corresponding Haskell code:"
      , ""
      , "```haskell"
      , source
      , "```"
      ]
  return CreateChatCompletion {
    messages = [ Message { role = User , content } ]
  , model = "deepseek-chat"
  , temperature = Just 0
  }

data Patch = Patch {
  strip :: Int
, diff :: String
} deriving (Eq, Show)

extractPatch :: Diagnostic -> String -> Maybe Patch
extractPatch diagnostic input = do
  diff <- extractDiffCodeBlock input
  span <- diagnostic.span
  file <- takeWhile (not . isSpace) . dropWhile isSpace <$> stripPrefix "--- " diff
  let strip = (length $ splitDirectories file) - (length $ splitDirectories span.file)
  Just Patch { strip, diff }

extractDiffCodeBlock :: String -> Maybe String
extractDiffCodeBlock = lines >>> \ case
  "```diff" : code -> case reverse code of
    "```" : (reverse >>> unlines -> diff) -> Just diff
    _ -> Nothing
  _ -> Nothing
