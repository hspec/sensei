{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Diagnostic (
  module Diagnostic
, annotate
, Action(..)
, analyze
, apply
#ifdef TEST
, variableNotInScope
, missingExtension
, redundantImport
, extractIdentifiers
, applyReplace
#endif
) where

import           Imports

import           System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (hPutBuilder)

import           GHC.Diagnostic.Type as Diagnostic
import           GHC.Diagnostic.Annotated

data Action = Choices [Action] | AddExtension FilePath Text | Replace Span Text
  deriving (Eq, Show)

annotate :: Diagnostic -> Annotated
annotate diagnostic = Annotated diagnostic annotation
  where
    annotation :: Maybe Annotation
    annotation = analyzeCode <|> analyzeHints

    analyzeCode :: Maybe Annotation
    analyzeCode = matchCode 66111 >> redundantImport

    matchCode :: Int -> Maybe ()
    matchCode expected = guard $ diagnostic.code == Just expected

    analyzeHints :: Maybe Annotation
    analyzeHints = head $ mapMaybe analyzeHint diagnostic.hints

    analyzeHint :: String -> Maybe Annotation
    analyzeHint (T.pack -> hint) =
          perhapsYouIntendedToUse
      <|> enableAnyOfTheFollowingExtensions
      <|> perhapsUse
      <|> perhapsUseOneOfThese
      where
        perhapsYouIntendedToUse :: Maybe Annotation
        perhapsYouIntendedToUse = T.stripPrefix "Perhaps you intended to use " hint >>= missingExtension . return . T.unpack

        enableAnyOfTheFollowingExtensions :: Maybe Annotation
        enableAnyOfTheFollowingExtensions = do
          T.stripPrefix "Enable any of the following extensions: " hint
            >>= missingExtension . map T.unpack . T.splitOn ", "

        perhapsUse :: Maybe Annotation
        perhapsUse = T.stripPrefix "Perhaps use `" hint
          >>= variableNotInScope . return . T.unpack . takeIdentifier 
          where
            takeIdentifier :: Text -> Text
            takeIdentifier = T.takeWhile (/= '\'')

        perhapsUseOneOfThese :: Maybe Annotation
        perhapsUseOneOfThese = do
          T.stripPrefix "Perhaps use one of these:" hint
          >>= variableNotInScope . map T.unpack . extractIdentifiers

variableNotInScope :: [FilePath] -> Maybe Annotation
variableNotInScope = Just . VariableNotInScopeAnnotation . VariableNotInScope

missingExtension :: [FilePath] -> Maybe Annotation
missingExtension = Just . MissingExtensionAnnotation . MissingExtension

redundantImport :: Maybe Annotation
redundantImport = Just $ RedundantImportAnnotation RedundantImport

_analyze :: Annotated -> Maybe Action
_analyze (Annotated diagnostic annotation) = foo
  where
    foo :: Maybe Action
    foo = case annotation of
      Just (RedundantImportAnnotation RedundantImport) -> removeLines
      Just (MissingExtensionAnnotation (MissingExtension extensions)) -> do
        file <- (.file) <$> diagnostic.span
        choices $ map (AddExtension file) (map T.pack extensions)

      Just (VariableNotInScopeAnnotation bar) -> do
          replaces <- Replace <$> diagnostic.span
          choices $ map (replaces . T.pack) bar.suggestions
      Nothing -> Nothing

    choices :: [Action] -> Maybe Action
    choices = \ case
      [] -> Nothing
      [action] -> Just action
      actions -> Just $ Choices actions

    removeLines :: Maybe Action
    removeLines = Replace <$> diagnosticLines <*> pure ""

    diagnosticLines :: Maybe Span
    diagnosticLines = do
      span <- diagnostic.span
      return span {
        start = Location span.start.line 1
      , end = Location (span.end.line + 1) 1
      }

analyze :: Diagnostic -> Maybe Action
analyze diagnostic = _analyze $ annotate diagnostic

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
