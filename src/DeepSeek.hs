{-# LANGUAGE CPP #-}
module DeepSeek (
  apply
#ifdef TEST
, createPrompt
, formatSpan
, Patch(..)
, extractPatch
#endif
) where

import           Imports hiding (strip)

import           Data.Ord
import           System.Process
import           System.Environment.Blank
import           Data.ByteString.Lazy (toStrict)
import           Data.Aeson
import qualified Data.Yaml.Pretty as Yaml
import qualified Data.Text as Text
import qualified Data.Text.IO.Utf8 as Utf8
import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Simple

import           Builder (Builder)
import qualified Builder
import           Sensei.API (Instructions(..))
import           GHC.Diagnostic (Diagnostic, Span, Location, FormatConfig(..))
import qualified GHC.Diagnostic.Type as Diagnostic
import qualified Config.DeepSeek as Config
import           DeepSeek.Types

separatorLength :: Int
separatorLength = 40

separator :: String
separator = take separatorLength $ repeat '#'

section :: String -> String
section name = take separatorLength $ take 3 separator ++ ' ' : name ++ ' ' : separator

apply :: (String -> IO ()) -> Config.DeepSeek -> FilePath -> These Diagnostic Instructions -> IO ()
apply putStrLn config dir instructions = case spanFromInstructions instructions of
  Nothing -> pass
  Just span -> do
    request <- createChatCompletion putStrLn dir span instructions
    response <- query putStrLn config request
    case response.choices of
      [] -> pass
      choice : _ -> do
        putStrLn $ section "making DeepSeek API request"
        case extractPatch span (unpack choice.message.content) of
          Nothing -> putStrLn $ section "no patch"
          Just patch -> applyPatch putStrLn dir patch
        putStrLn separator

spanFromInstructions :: These Diagnostic Instructions -> Maybe Span
spanFromInstructions = \ case
  This diagnostic -> diagnostic.span
  That instructions -> Just instructions.span
  These _ instructions -> Just instructions.span

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

createPrompt :: FilePath -> Span -> These Diagnostic Instructions -> IO Text
createPrompt dir span instructions = do
  source <- Builder.fromText <$> Utf8.readFile (dir </> span.file)
  let
    aDiagnosticsMessageAndProgrammerInstructions :: Builder
    aDiagnosticsMessageAndProgrammerInstructions = case instructions of
      This (_ :: Diagnostic) -> aDiagnosticsMessage
      That (_ :: Instructions) -> instructionsProvidedByAProgrammer
      These (_ :: Diagnostic) (_ :: Instructions) -> aDiagnosticsMessage <> " and " <> instructionsProvidedByAProgrammer
      where
        aDiagnosticsMessage = "a GHC diagnostics message"
        instructionsProvidedByAProgrammer = "instructions provided by a programmer"

    diagnosticsMessage :: [Builder]
    diagnosticsMessage = case this instructions of
      Nothing -> []
      Just diagnostic -> [
          ""
        , "The GHC diagnostics message:"
        , ""
        , "```console"
        , Builder.fromText . Text.stripEnd . pack $ Diagnostic.format formatConfig diagnostic
        , "```"
        ]

    formatConfig :: FormatConfig
    formatConfig = FormatConfig { showErrorContext = True, color = False }

    programmerInstructions :: [Builder]
    programmerInstructions = case that instructions of
      Nothing -> []
      Just programmer -> [
          ""
        , "The instructions provided by the programmer: " <> Builder.fromText programmer.instructions
        , ""
        , "The programmer is currently focusing on: " <> formatSpan span
        ]

    diagnosticsMessageAndProgrammerInstructions :: [Builder]
    diagnosticsMessageAndProgrammerInstructions = diagnosticsMessage ++ programmerInstructions

    prompt :: Text
    prompt = Builder.toText . Builder.join "\n" $ [
        "Given " <> aDiagnosticsMessageAndProgrammerInstructions <> ", please suggest a fix for the corresponding Haskell code."
      , ""
      , "Produce your answer as a unified diff so that it can be applied with the `patch` program."
      , "Don't provide explanations."
      , ""
      , "Enclose your answer in:"
      , ""
      , "```diff"
      , "--- " <> fromString span.file
      , "+++ " <> fromString span.file
      , "..."
      , "```"
      , ""
      , "(where ... is the placeholder for your answer)"
      , Builder.join "\n" diagnosticsMessageAndProgrammerInstructions
      , ""
      , "The corresponding Haskell code:"
      , ""
      , "```haskell"
      , source
      , "```"
      , ""
      ]
  return prompt

formatSpan :: Span -> Builder
formatSpan span
  | start == end = file <> ":" <> startLine <> ":" <> startColumn
  | start.line == end.line = file <> ":" <> startLine <> ":" <> startColumn <> "-" <> endColumn
  | otherwise = file <> ":(" <> startLine <> "," <> startColumn <> "-" <> endLine <> "," <> endColumn <> ")"
  where
    start :: Location
    start = span.start

    end :: Location
    end = span.end

    file :: Builder
    file = fromString span.file

    startLine :: Builder
    startLine = Builder.show start.line

    startColumn :: Builder
    startColumn = Builder.show start.column

    endLine :: Builder
    endLine = Builder.show end.line

    endColumn :: Builder
    endColumn = Builder.show end.column

createChatCompletion :: (String -> IO ()) -> FilePath -> Span -> These Diagnostic Instructions -> IO CreateChatCompletion
createChatCompletion putStrLn dir span instructions = do
  prompt <- createPrompt dir span instructions
  putStrLn $ section "prompt"
  putStrLn $ Text.unpack prompt
  return CreateChatCompletion {
    messages = [ Message { role = User, content = prompt } ]
  , model = "deepseek-chat"
  , temperature = Just 0
  }

data Patch = Patch {
  strip :: Int
, diff :: String
} deriving (Eq, Show)

extractPatch :: Span -> String -> Maybe Patch
extractPatch span input = do
  diff <- extractDiffCodeBlock input
  file <- takeWhile (not . isSpace) . dropWhile isSpace <$> stripPrefix "--- " diff
  let strip = (length $ splitDirectories file) - (length $ splitDirectories span.file)
  Just Patch { strip, diff }

extractDiffCodeBlock :: String -> Maybe String
extractDiffCodeBlock = lines >>> \ case
  "```diff" : code -> case reverse code of
    "```" : (reverse >>> unlines -> diff) -> Just diff
    _ -> Nothing
  _ -> Nothing
