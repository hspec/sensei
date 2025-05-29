{-# LANGUAGE CPP #-}
module GHC.Diagnostic (
  module Diagnostic
, Annotated(..)
, AvailableImports
, parseAnnotated
, formatAnnotated

, Edit
, edits
, apply

#ifdef TEST
, analyzeHint
, extractIdentifiers
, qualifiedName
, applyReplace
#endif
) where

import           Imports hiding (stripPrefix, takeExtensions)
import           Builder (Builder, Color(..))
import qualified Builder

import           System.IO
import           Data.Text (stripPrefix, stripSuffix)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Builder (hPutBuilder)
import           Data.Map (Map)
import qualified Data.Map as Map

import           GHC.Diagnostic.Type as Diagnostic
import           GHC.Diagnostic.Annotated
import           GHC.Diagnostic.Util

formatAnnotated :: Annotated -> Text
formatAnnotated annotated = Builder.toText $
  fromString (format annotated.diagnostic) <> formatSolutions annotated.solutions

formatSolutions :: [Solution] -> Builder
formatSolutions = Builder.unlines . zipWith formatNumbered [1..]
  where
    formatNumbered :: Int -> Solution -> Builder
    formatNumbered n solution = formatNumber n <> formatSolution solution

    formatNumber :: Int -> Builder
    formatNumber n = Builder.withColor Cyan $ "    " <> "[" <> Builder.show n <> "] "

    formatSolution :: Solution -> Builder
    formatSolution = \ case
      EnableExtension name -> "Enable " <> Builder.fromText name
      RemoveImport -> "Remove import"
      UseName name -> "Use " <> Builder.fromText name
      ImportName module_ qualification name -> importStatement module_ qualification [name]

type AvailableImports = Map Text [Module]

parseAnnotated :: AvailableImports -> ByteString -> Maybe Annotated
parseAnnotated availableImports = parse >>> (<&> annotate availableImports)

annotate :: AvailableImports -> Diagnostic -> Annotated
annotate availableImports diagnostic = Annotated { diagnostic, annotation, solutions }
  where
    annotation :: Maybe Annotation
    annotation = parseAnnotation diagnostic

    solutions :: [Solution]
    solutions =
         analyzeHints diagnostic.hints
      ++ maybe [] (analyzeAnnotation availableImports) annotation

analyzeHints :: [Text] -> [Solution]
analyzeHints = concat . mapMaybe analyzeHint

analyzeHint :: Text -> Maybe [Solution]
analyzeHint hint = asum [
    prefix "Perhaps you intended to use " <&> takeExtensions

  , requiredFor GHC_910 $ prefix "Enable any of the following extensions: " <&>
      map EnableExtension . reverse . T.splitOn ", "

  , prefix "Perhaps use `" <&> return . takeIdentifier
  , prefix "Perhaps use one of these:" <&> extractIdentifiers
  ]
  where
    prefix :: Text -> Maybe Text
    prefix p = stripPrefix p hint

    takeExtensions :: Text -> [Solution]
    takeExtensions input = fromMaybe takeExtensionGhc910 takeExtensionGhc912
      where
        takeExtensionGhc910 :: [Solution]
        takeExtensionGhc910 = requiredFor GHC_910 [EnableExtension input]

        takeExtensionGhc912 :: Maybe [Solution]
        takeExtensionGhc912 = map EnableExtension <$> do
          T.stripPrefix "the `" input <&> T.span (/= '\'') >>= \ case
            (extension, "' extension") -> Just [extension]
            (implied, impliedBy -> Just extension) -> Just [implied, extension]
            _ -> Nothing
          where
            impliedBy = T.stripPrefix "' extension (implied by `" >=> T.stripSuffix "')"

    takeIdentifier :: Text -> Solution
    takeIdentifier = UseName . T.takeWhile (/= '\'')

extractIdentifiers :: Text -> [Solution]
extractIdentifiers input = case T.breakOn "`" >>> snd >>> T.breakOn "\'" $ input of
  (T.drop 1 -> identifier, rest)
    | T.null rest -> []
    | otherwise -> UseName identifier : extractIdentifiers rest

parseAnnotation :: Diagnostic -> Maybe Annotation
parseAnnotation diagnostic = asum [
    matchCode 66111 $> RedundantImport
  , analyzeMessage
  ]
  where
    matchCode :: Int -> Maybe ()
    matchCode expected = guard $ diagnostic.code == Just expected

    message :: [Text]
    message = map joinMessageLines diagnostic.message

    analyzeMessage :: Maybe Annotation
    analyzeMessage = asum . map analyzeMessageLine $ concatMap T.lines message

    analyzeMessageLine :: Text -> Maybe Annotation
    analyzeMessageLine input = NotInScope <$> asum [
        variableNotInScope
      , qualifiedNameNotInScope
      ]
      where
        prefix :: Text -> Maybe Text
        prefix p = stripPrefix p input

        variableNotInScope :: Maybe RequiredVariable
        variableNotInScope = prefix "Variable not in scope: " <&> variable

        qualifiedNameNotInScope :: Maybe RequiredVariable
        qualifiedNameNotInScope = prefix "Not in scope: `" >>= stripSuffix "'" <&> qualifiedName

        variable :: Text -> RequiredVariable
        variable = breakOn " :: " >>> \ case
          (name, "") -> RequiredVariable Unqualified name NoTypeSignature
          (name, type_) -> RequiredVariable Unqualified name (TypeSignature $ Type type_)

qualifiedName :: Text -> RequiredVariable
qualifiedName = breakOnEnd "." >>> \ case
  ("", name) -> RequiredVariable Unqualified name NoTypeSignature
  (qualification, name) -> RequiredVariable (Qualified qualification) name NoTypeSignature

breakOn :: Text -> Text -> (Text, Text)
breakOn sep = second (T.drop $ T.length sep) . T.breakOn sep

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd sep = first (T.dropEnd $ T.length sep) . T.breakOnEnd sep

analyzeAnnotation :: AvailableImports -> Annotation -> [Solution]
analyzeAnnotation availableImports = \ case
  RedundantImport -> [RemoveImport]
  NotInScope name -> importRequired name
  where
    importRequired :: RequiredVariable -> [Solution]
    importRequired required = map importName $ sortImports required modules
      where
        modules :: [Module]
        modules = fromMaybe [] (Map.lookup required.name availableImports)

        importName :: Module -> Solution
        importName module_ = ImportName module_ required.qualification required.name

data Edit =
    AddExtension FilePath Text
  | AddImport FilePath Module Qualification [Text]
  | Replace Span Text
  deriving (Eq, Show)

edits :: Annotated -> [Edit]
edits annotated = case annotated.diagnostic.span of
  Nothing -> []
  Just span -> map toEdit annotated.solutions
    where
      toEdit :: Solution -> Edit
      toEdit = \ case
        EnableExtension name -> AddExtension file name
        RemoveImport -> removeLines
        UseName name -> Replace span name
        ImportName module_ qualification name -> AddImport file module_ qualification [name]

      file :: FilePath
      file = span.file

      removeLines :: Edit
      removeLines = Replace diagnosticLines ""

      diagnosticLines :: Span
      diagnosticLines = span {
        start = Location span.start.line 1
      , end = Location (span.end.line + 1) 1
      }

apply :: FilePath -> Maybe Int -> [Edit] -> IO ()
apply dir choice = selectChoice >>> applyChoice
  where
    selectChoice :: [Edit] -> Maybe Edit
    selectChoice = drop (maybe 0 pred choice) >>> head

    applyChoice :: Maybe Edit -> IO ()
    applyChoice = maybe pass (relativeTo dir >>> applyEdit)

relativeTo :: FilePath -> Edit -> Edit
relativeTo dir = \ case
  AddExtension file name -> AddExtension (dir </> file) name
  AddImport file module_ qualification names -> AddImport (dir </> file) module_ qualification names
  Replace span substitute -> Replace span { file = dir </> span.file } substitute

applyEdit :: Edit -> IO ()
applyEdit = \ case
  AddExtension file name -> do
    prependToFile file $ "{-# LANGUAGE " <> name <> " #-}\n"

  AddImport file module_ qualification names -> do
    modifyFile file $ addImport (importStatement module_ qualification names)

  Replace span substitute -> do
    modifyFile span.file $ applyReplace span.start span.end substitute

addImport :: Builder -> [ByteString] -> [ByteString]
addImport statement input = case break (B.isPrefixOf "import ") input of
  (xs, ys) -> xs ++ Builder.toByteString statement : ys

importStatement :: Module -> Qualification -> [Text] -> Builder
importStatement (Module (Builder.fromText -> module_)) qualification names = case qualification of
  Unqualified -> "import " <> module_ <> " (" <> Builder.join ", " (map Builder.fromText names) <> ")"
  Qualified name -> "import " <> module_ <> " qualified as " <> Builder.fromText name

applyReplace :: Location -> Location -> Text -> [ByteString] -> [ByteString]
applyReplace start end substitute input =
  let
    (before, rest) = splitAt (start.line - 1) input

    after :: [ByteString]
    after = drop (end.line - start.line + 1) rest

    decodedLines :: [Text]
    decodedLines = map T.decodeUtf8Lenient rest
  in case do
    firstLine <- head decodedLines
    lastLine <- head $ drop (end.line - start.line) decodedLines
    return $ mconcat [T.take (start.column - 1) firstLine, substitute, T.drop (end.column - 1) lastLine]
  of
    Nothing -> input
    Just substituted -> before ++ T.encodeUtf8 substituted : after

prependToFile :: FilePath -> Text -> IO ()
prependToFile file contents = do
  old <- B.readFile file
  withFile file WriteMode $ \ h -> do
    hPutBuilder h $ T.encodeUtf8Builder contents
    B.hPutStr h old

modifyFile :: FilePath -> ([ByteString] -> [ByteString]) -> IO ()
modifyFile file f = do
  old <- B.lines <$> B.readFile file
  B.writeFile file . B.unlines $ f old
