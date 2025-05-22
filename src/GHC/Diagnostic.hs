{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Diagnostic (
  module Diagnostic
, Action(..)
, analyze
, apply
#ifdef TEST
, extractIdentifiers
, applyReplace
, joinMessageLines
#endif
) where

import           Imports

import           System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (hPutBuilder)

import           GHC.Diagnostic.Type as Diagnostic

data Action = Choices [Action] | AddExtension FilePath Text | Replace Span Text
  deriving (Eq, Show)

analyze :: Diagnostic -> Maybe Action
analyze diagnostic = analyzeCode <|> analyzeHints
  where
    _message :: [Text]
    _message = map joinMessageLines diagnostic.message

    analyzeCode :: Maybe Action
    analyzeCode = redundantImport
      where
        redundantImport :: Maybe Action
        redundantImport = matchCode 66111 >> removeLines

    matchCode :: Int -> Maybe ()
    matchCode expected = guard $ diagnostic.code == Just expected

    removeLines :: Maybe Action
    removeLines = Replace <$> diagnosticLines <*> pure ""

    diagnosticLines :: Maybe Span
    diagnosticLines = do
      span <- diagnostic.span
      return span {
        start = Location span.start.line 1
      , end = Location (span.end.line + 1) 1
      }

    analyzeHints :: Maybe Action
    analyzeHints = head $ mapMaybe analyzeHint diagnostic.hints

    analyzeHint :: Text -> Maybe Action
    analyzeHint hint =
          perhapsYouIntendedToUse
      <|> enableAnyOfTheFollowingExtensions
      <|> perhapsUse
      <|> perhapsUseOneOfThese
      where
        perhapsYouIntendedToUse :: Maybe Action
        perhapsYouIntendedToUse = do
          AddExtension . (.file) <$> diagnostic.span <*> T.stripPrefix "Perhaps you intended to use " hint

        enableAnyOfTheFollowingExtensions :: Maybe Action
        enableAnyOfTheFollowingExtensions = do
          file <- (.file) <$> diagnostic.span
          T.stripPrefix "Enable any of the following extensions: " hint
            >>= head . reverse . map (AddExtension file) . T.splitOn ", "

        perhapsUse :: Maybe Action
        perhapsUse = Replace <$> diagnostic.span <*> (takeIdentifier <$> T.stripPrefix "Perhaps use `" hint)
          where
            takeIdentifier :: Text -> Text
            takeIdentifier = T.takeWhile (/= '\'')

        perhapsUseOneOfThese :: Maybe Action
        perhapsUseOneOfThese = do
          replaces <- Replace <$> diagnostic.span
          Choices . map replaces . extractIdentifiers <$> T.stripPrefix "Perhaps use one of these:" hint

extractIdentifiers :: Text -> [Text]
extractIdentifiers input = case T.breakOn "`" >>> snd >>> T.breakOn "\'" $ input of
  (T.drop 1 -> identifier, rest)
    | T.null rest -> []
    | otherwise -> identifier : extractIdentifiers rest

apply :: FilePath -> Maybe Int -> Action -> IO ()
apply dir c = relativeTo dir >>> go c
  where
    go :: Maybe Int -> Action -> IO ()
    go choice = \ case
      Choices choices -> do
        traverse_ (go Nothing) (head $ drop (maybe 0 pred choice) choices)
      AddExtension file name -> do
        old <- B.readFile file
        withFile file WriteMode $ \ h -> do
          hPutBuilder h $ "{-# LANGUAGE " <> T.encodeUtf8Builder name <> " #-}\n"
          B.hPutStr h old
      Replace span substitute -> do
        input <- B.readFile span.file <&> B.lines
        B.writeFile span.file . B.unlines $
          applyReplace span.start span.end substitute input

relativeTo :: FilePath -> Action -> Action
relativeTo dir = \ case
  Choices choices -> Choices $ map (relativeTo dir) choices
  AddExtension file name -> AddExtension (dir </> file) name
  Replace span substitute -> Replace span { file = dir </> span.file } substitute

applyReplace :: Location -> Location -> Text -> [ByteString] -> [ByteString]
applyReplace start end substitute input =
  let
    (before, rest) = splitAt (start.line - 1) input

    after :: [ByteString]
    after = drop (end.line - start.line + 1) rest

    decodedLines :: [Text]
    decodedLines = map T.decodeUtf8Lenient rest
  in case do
    firstLine <- head $ decodedLines
    lastLine <- head $ drop (end.line - start.line) decodedLines
    return $ T.take (start.column - 1) firstLine <> substitute <> T.drop (end.column - 1) lastLine
  of
    Nothing -> input
    Just substituted -> before ++ T.encodeUtf8 substituted : after

joinMessageLines :: Text -> Text
joinMessageLines = T.intercalate "\n" . loop . T.splitOn "\n"
  where
    loop :: [Text] -> [Text]
    loop = \ case
      [] -> []
      x : (T.span isSpace -> (T.null -> False, y)) : ys -> loop $ (x <> " " <> y) : ys
      x : xs -> x : loop xs
