{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Diagnostic (
  module Diagnostic

, Annotated(..)

, IdentifierMap
, Identifier(..)
, SuggestIdentifier(..)
, Module(..)

, annotate
, parseAnnotated
, formatAnnotated

, Action(..)
, analyze
, apply

#ifdef TEST
, sortSuggestions
, variableNotInScope
, missingExtension
, redundantImport
, extractIdentifiers
, applyReplace
#endif
) where

import           Data.Ord
import qualified Data.List as List
import           Imports hiding (mod)

import           System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (Builder, hPutBuilder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L

import           GHC.Diagnostic.Type as Diagnostic
import           GHC.Diagnostic.Annotated

import           Data.Map (Map)
import qualified Data.Map as Map

type IdentifierMap = Map Text [Identifier]

parseAnnotated :: IdentifierMap -> ByteString -> Maybe Annotated
parseAnnotated identifierMap = fmap (annotate identifierMap) . parse

formatAnnotation :: Annotation -> String
formatAnnotation = \ case
  VariableNotInScopeAnnotation annotation -> unlines $ map suggestImport (zip [1..] annotation.suggestions)
  RedundantImportAnnotation RedundantImport -> "1. remove import" <> "\n"
  MissingExtensionAnnotation annotation -> unlines $ map suggestExtension (zip [1..] annotation.extensions)
  where
    formatNumber :: Int -> String
    formatNumber n = "    \x1b[36m[" <> show n <> "]\x1b[0m "

    suggestExtension :: (Int, Text) -> String
    suggestExtension (n, xs) = formatNumber n <> "Enable " <> T.unpack xs

    suggestImport :: (Int, SuggestIdentifier) -> String
    suggestImport (n, xs) = formatNumber n <> case xs of
      SuggestIdentifier (Identifier (Module name) _) -> "Import " <> T.unpack name
      IdentifierInScope name -> "Use " <> T.unpack name

-- foo = bar

formatAnnotated :: Annotated -> String
formatAnnotated diagnostic = format diagnostic.diagnostic <> maybe "" formatAnnotation diagnostic.annotation

data Action = AddExtension FilePath Text | AddImport FilePath Module Qualification [Text] | Replace Span Text
  deriving (Eq, Show)

annotate :: IdentifierMap -> Diagnostic -> Annotated
annotate identifierMap diagnostic = Annotated diagnostic $ 
      analyzeCode
  <|> analyzeHints
  <|> notInScope
  where
    message :: [String]
    message = map joinMessageLines diagnostic.message

    hints :: [Text]
    hints = map T.pack diagnostic.hints

    analyzeCode :: Maybe Annotation
    analyzeCode = matchCode 66111 >> redundantImport

    matchCode :: Int -> Maybe ()
    matchCode expected = guard $ diagnostic.code == Just expected

    analyzeHints :: Maybe Annotation
    analyzeHints = head $ mapMaybe analyzeHint hints

    notInScope :: Maybe Annotation
    notInScope = do
      name <- case message of
        [x] -> case lines x of
          [y] -> extractVariableNotInScope y
          [y, _] -> extractVariableNotInScope_ y
          _ -> Nothing
        _ -> Nothing
      let
        fromEnvironment :: [SuggestIdentifier]
        fromEnvironment = map SuggestIdentifier . sortSuggestions name $ fromMaybe [] (Map.lookup name.name identifierMap)

      variableNotInScope name $ fromHints ++ fromEnvironment
      where
        fromHints :: [SuggestIdentifier]
        fromHints = concat $ mapMaybe baz hints
        
    baz :: Text -> Maybe [SuggestIdentifier]
    baz hint = perhapsUseOneOfThese <|> perhapsUse
      where
        perhapsUseOneOfThese :: Maybe [SuggestIdentifier]
        perhapsUseOneOfThese = T.stripPrefix "Perhaps use one of these:" hint <&> extractIdentifiers

        perhapsUse :: Maybe [SuggestIdentifier]
        perhapsUse = T.stripPrefix "Perhaps use `" hint <&> return . takeIdentifier 
          where
            takeIdentifier :: Text -> SuggestIdentifier
            takeIdentifier = IdentifierInScope . T.takeWhile (/= '\'')


    extractVariableNotInScope :: String -> Maybe RequiredVariable
    extractVariableNotInScope x = do
      r <- T.stripPrefix "Variable not in scope: " (T.pack x)
      return case breakOn " :: " r of
        (name, "") -> RequiredVariable Unqualified name Nothing
        (name, Just -> type_) -> RequiredVariable Unqualified name type_

    extractVariableNotInScope_ :: String -> Maybe RequiredVariable
    extractVariableNotInScope_ x = do
      r <- T.stripPrefix "Not in scope: `" (T.pack x)
      q <- T.stripSuffix "'" r
      return case breakOnEnd "." q of
        (qualification, name) -> RequiredVariable (Qualified qualification) name Nothing

    breakOn :: Text -> Text -> (Text, Text)
    breakOn sep = fmap (T.drop $ T.length sep) . T.breakOn sep

    breakOnEnd :: Text -> Text -> (Text, Text)
    breakOnEnd sep = fmap (T.drop $ T.length sep) . T.breakOn sep

    analyzeHint :: Text -> Maybe Annotation
    analyzeHint hint =
          perhapsYouIntendedToUse
      <|> enableAnyOfTheFollowingExtensions
      where
        perhapsYouIntendedToUse :: Maybe Annotation
        perhapsYouIntendedToUse = T.stripPrefix "Perhaps you intended to use " hint >>= missingExtension . return

        enableAnyOfTheFollowingExtensions :: Maybe Annotation
        enableAnyOfTheFollowingExtensions = do
          T.stripPrefix "Enable any of the following extensions: " hint
            >>= missingExtension . T.splitOn ", "

sortSuggestions :: RequiredVariable ->[Identifier] -> [Identifier]
sortSuggestions name = case name.qualification of
  Unqualified -> sortOn (.module_)
  Qualified qualification -> sortOn f
    where
      f (Identifier (Module module_) _name) = (
          Down $ qualification `elem` moduleComponents
        , Down $ any (T.isPrefixOf qualification) moduleComponents
        , map toModuleComponent moduleComponents)
        where
          moduleComponents :: [Text]
          moduleComponents = T.splitOn "." module_

data ModuleComponent = ModuleComponent Text | InternalModuleComponent Text
  deriving Eq

toModuleComponent :: Text -> ModuleComponent
toModuleComponent name = case name of
  "Internal" -> InternalModuleComponent name
  _ -> ModuleComponent name

instance Ord ModuleComponent where
  compare (ModuleComponent a) (ModuleComponent b) = compare a b
  compare (InternalModuleComponent a) (InternalModuleComponent b) = compare a b
  compare (InternalModuleComponent _) (ModuleComponent _) = GT
  compare (ModuleComponent _) (InternalModuleComponent _) = LT

variableNotInScope :: RequiredVariable -> [SuggestIdentifier] -> Maybe Annotation
variableNotInScope name = Just . VariableNotInScopeAnnotation . VariableNotInScope name

missingExtension :: [Text] -> Maybe Annotation
missingExtension = Just . MissingExtensionAnnotation . MissingExtension

redundantImport :: Maybe Annotation
redundantImport = Just $ RedundantImportAnnotation RedundantImport

analyze :: Annotated -> [Action]
analyze (Annotated diagnostic annotation) = case diagnostic.span of
  Nothing -> []
  Just span -> maybe [] (analyzeAnnotation span) annotation

analyzeAnnotation :: Span -> Annotation -> [Action]
analyzeAnnotation span annotation = case annotation of
  RedundantImportAnnotation RedundantImport -> [removeLines]
  MissingExtensionAnnotation (MissingExtension extensions) -> map (AddExtension file) extensions
  VariableNotInScopeAnnotation notInScope -> map xxx notInScope.suggestions
    where
      xxx :: SuggestIdentifier -> Action
      xxx = \ case
        IdentifierInScope name -> 
          Replace span name
        SuggestIdentifier (Identifier mod name) ->
          AddImport file mod notInScope.name.qualification [T.pack name]
    where
      file :: FilePath
      file = span.file

      removeLines :: Action
      removeLines = Replace diagnosticLines ""

      diagnosticLines :: Span
      diagnosticLines = span {
        start = Location span.start.line 1
      , end = Location (span.end.line + 1) 1
      }

extractIdentifiers :: Text -> [SuggestIdentifier]
extractIdentifiers input = case T.breakOn "`" >>> snd >>> T.breakOn "\'" $ input of
  (T.drop 1 -> identifier, rest)
    | T.null rest -> []
    | otherwise -> IdentifierInScope identifier : extractIdentifiers rest

apply :: FilePath -> Maybe Int -> [Action] -> IO ()
apply dir c = map (relativeTo dir) >>> (head . drop (maybe 0 pred c)) >>> maybe pass go
  where
    go :: Action -> IO ()
    go = \ case
      AddExtension file name -> do
        old <- B.readFile file
        withFile file WriteMode $ \ h -> do
          hPutBuilder h $ "{-# LANGUAGE " <> T.encodeUtf8Builder name <> " #-}\n"
          B.hPutStr h old

      AddImport file mod qualification names -> do
        old <- B.lines <$> B.readFile file
        B.writeFile file . B.unlines $ addImport (importStatement mod qualification names) old

      Replace span substitute -> do
        input <- B.readFile span.file <&> B.lines
        B.writeFile span.file . B.unlines $
          applyReplace span.start span.end substitute input

importStatement :: Module -> Qualification -> [Text] -> Builder
importStatement (Module mod) qualification names = case qualification of
  Unqualified -> "import " <> T.encodeUtf8Builder mod <> " (" <> T.encodeUtf8Builder (T.intercalate ", " names) <> ")"
  Qualified name -> "import " <> T.encodeUtf8Builder mod <> " qualified as " <> T.encodeUtf8Builder name

relativeTo :: FilePath -> Action -> Action
relativeTo dir = \ case
  AddExtension file name -> AddExtension (dir </> file) name
  AddImport file qualification mod names -> AddImport (dir </> file) qualification mod names
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

addImport :: Builder -> [ByteString] -> [ByteString]
addImport module_ = add
  where
    add :: [ByteString] -> [ByteString]
    add input = case break (B.isPrefixOf "import ") input of
      (xs, ys) -> xs ++ L.toStrict (Builder.toLazyByteString module_) : ys

joinMessageLines :: String -> String
joinMessageLines = T.unpack . T.intercalate "\n" . join_ . T.splitOn "\n" . T.pack
  where
    join_ :: [Text] -> [Text]
    join_ = map T.pack . joinLines . map T.unpack

joinLines :: [String] -> [String]
joinLines = go
  where
    go :: [String] -> [String]
    go = \ case
      [] -> []
      x : (List.span isSpace -> (_ : _, y)) : ys -> go $ (x <> " " <> y) : ys
      x : xs -> x : go xs
