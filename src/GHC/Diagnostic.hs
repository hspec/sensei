{-# LANGUAGE CPP #-}
module GHC.Diagnostic (
  module Diagnostic
, Annotated(..)
, Name(..)
, NameSpace(..)
, AvailableImports
, ProvidedBy(..)
, parseAnnotated
, formatAnnotated

, Edit
, edits
, apply

#ifdef TEST
, analyzeHint
, extractIdentifiers
, qualifiedName
, analyzeAnnotation
, applyReplace
#endif
) where

import           Imports hiding (stripPrefix, takeExtensions)
import           Builder (Builder)
import qualified Builder
import           System.Console.ANSI.Types

import           System.IO
import qualified Data.List as List
import           Data.Text (stripPrefix, stripSuffix)
import qualified Data.Text as T hiding (stripPrefix, stripSuffix)
import qualified Data.Text.IO.Utf8 as Utf8
import           Data.Map (Map)
import qualified Data.Map as Map

import           GHC.Diagnostic.Type as Diagnostic
import           GHC.Diagnostic.Annotated
import           GHC.Diagnostic.Util

formatAnnotated :: FormatConfig -> Int -> Annotated -> (Int, Text)
formatAnnotated config start annotated = case formatSolutions start annotated.solutions of
  (next, solutions) -> (next, Builder.toText $ fromString (format config annotated.diagnostic) <> solutions)

formatSolutions :: Int -> [Solution] -> (Int, Builder)
formatSolutions start = zipWith formatNumbered [start..] >>> reverse >>> \ case
  [] -> (start, mempty)
  solutions@((succ -> next, _) : _) -> (next, Builder.unlines (reverse $ map snd solutions) <> "\n")
  where
    formatNumbered :: Int -> Solution -> (Int, Builder)
    formatNumbered n solution = (n, formatNumber n <> formatSolution solution)

    formatNumber :: Int -> Builder
    formatNumber n = highlight $ "    " <> "[" <> Builder.show n <> "] "

    highlight :: Builder -> Builder
    highlight = Builder.withSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]

    formatSolution :: Solution -> Builder
    formatSolution = \ case
      EnableExtension name -> "Enable " <> Builder.fromText name
      RemoveImport -> "Remove import"
      UseName name -> "Use " <> Builder.fromText name
      ImportName module_ qualification name -> importStatement module_ qualification [name] <> faint package
        where
          package = " (" <> Builder.fromText module_.package.name <> ")"

    faint :: Builder -> Builder
    faint = Builder.withSGR [SetConsoleIntensity FaintIntensity]

data ProvidedBy = ProvidedBy {
  module_ :: Module
, type_ :: Maybe Type
} deriving (Eq, Show)

type AvailableImports = Map Name [ProvidedBy]

parseAnnotated :: IO AvailableImports -> ByteString -> IO (Maybe Annotated)
parseAnnotated getAvailableImports input = case parse input of
  Nothing -> return Nothing
  Just diagnostic -> Just <$> annotate getAvailableImports diagnostic

annotate :: IO AvailableImports -> Diagnostic -> IO Annotated
annotate getAvailableImports diagnostic = getAvailableImports >>= \ case
  availableImports -> return $ Annotated { diagnostic, annotation, solutions }
    where
      annotation :: Maybe Annotation
      annotation = parseAnnotation diagnostic

      solutions :: [Solution]
      solutions =
           analyzeHints diagnostic.message
        ++ analyzeHints diagnostic.hints
        ++ maybe [] (analyzeAnnotation availableImports) annotation

analyzeHints :: [Text] -> [Solution]
analyzeHints = concat . mapMaybe analyzeHint

analyzeHint :: Text -> Maybe [Solution]
analyzeHint hint = asum [
    prefix "Perhaps you intended to use " <&> takeExtensions

  , requiredFor GHC_910 $ prefix "Enable any of the following extensions: " <&>
      map EnableExtension . reverse . T.splitOn ", "

  , prefix "Perhaps use `" <&> return . takeIdentifier
  , prefix "Perhaps use variable `" <&> return . takeIdentifier
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
          stripPrefix "the `" input <&> T.span (/= '\'') >>= \ case
            (extension, "' extension") -> Just [extension]
            (implied, impliedBy -> Just extension) -> Just [implied, extension]
            _ -> Nothing
          where
            impliedBy = stripPrefix "' extension (implied by `" >=> stripSuffix "')"

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
    analyzeMessageLine input = asum [
        VariableNotInScope <$> variableNotInScope
      , VariableNotInScope <$> qualifiedNameNotInScope
      , VariableNotInScope <$> dataConstructorNotInScope
      , VariableNotInScope <$> dataConstructorNotInScopeInPattern
      , termLevelUseOfTypeConstructor
      , typeConstructorNotInScope
      , foundHole
      ]
      where
        foundHole :: Maybe Annotation
        foundHole = FoundHole <$> (prefix "Found hole: _" >>= takeTypeSignature) <*> analyzeHoleFits

        prefix :: Text -> Maybe Text
        prefix p = stripPrefix p input

        match :: Text -> Maybe Text
        match t = prefix (t <> " `") >>= stripSuffix "'"

        variableNotInScope :: Maybe RequiredVariable
        variableNotInScope = prefix "Variable not in scope: " <&> qualifiedName

        qualifiedNameNotInScope :: Maybe RequiredVariable
        qualifiedNameNotInScope = match "Not in scope:" <&> qualifiedName

        dataConstructorNotInScope :: Maybe RequiredVariable
        dataConstructorNotInScope = prefix "Data constructor not in scope: " <&> qualifiedName

        dataConstructorNotInScopeInPattern :: Maybe RequiredVariable
        dataConstructorNotInScopeInPattern = match "Not in scope: data constructor" <&> qualifiedName

        termLevelUseOfTypeConstructor :: Maybe Annotation
        termLevelUseOfTypeConstructor = match "Illegal term-level use of the type constructor"
          <&> qualified TermLevelUseOfTypeConstructor

        typeConstructorNotInScope :: Maybe Annotation
        typeConstructorNotInScope = match "Not in scope: type constructor or class" <&> qualified TypeNotInScope

    analyzeHoleFits :: Maybe [HoleFit]
    analyzeHoleFits = asum $ map validHoleFitsInclude diagnostic.message

    validHoleFitsInclude :: Text -> Maybe [HoleFit]
    validHoleFitsInclude (T.lines -> input) = oneLine <|> multiline
      where
        prefix :: Text
        prefix = "Valid hole fits include"

        oneLine :: Maybe [HoleFit]
        oneLine = return . holeFit <$> asum do
          map (stripPrefix $ prefix <> " ") input

        multiline :: Maybe [HoleFit]
        multiline = case List.break (== prefix) input of
          (_, m : matches) | m == prefix -> Just . map holeFit $ joinHoleFits matches
          _ -> Nothing
          where
            joinHoleFits :: [Text] -> [Text]
            joinHoleFits = discardBoring . joinLines 3 . mapMaybe (stripPrefix "  ")

            discardBoring :: [Text] -> [Text]
            discardBoring = filter isBoring

            isBoring :: Text -> Bool
            isBoring = T.uncons >>> \ case
              Nothing -> False
              Just (c, _) -> c /= '(' && c /= ' '

        holeFit :: Text -> HoleFit
        holeFit = breakOn " (bound at " >>> fst >>> T.strip >>> breakOnTypeSignature HoleFit

    takeTypeSignature :: Text -> Maybe Type
    takeTypeSignature t = case breakOn " :: " t of
      (_, "") -> Nothing
      (_, type_) -> Just (Type type_)

breakOnTypeSignature :: (Text -> TypeSignature -> a) -> Text -> a
breakOnTypeSignature c t = case breakOn " :: " t of
  (name, "") -> c name NoTypeSignature
  (name, type_) -> c name (TypeSignature $ Type type_)

qualifiedName :: Text -> RequiredVariable
qualifiedName = breakOnTypeSignature $ qualified RequiredVariable

qualified :: (Qualification -> Text -> t) -> Text -> t
qualified c input = case stripParensAroundOperators input of
  Just operator -> c Unqualified operator
  Nothing -> case breakOnEnd "." input of
    ("", name) -> c Unqualified name
    (qualification, name) -> c (Qualified qualification) name

stripParensAroundOperators :: Text -> Maybe Text
stripParensAroundOperators = stripPrefix "(" >=> stripSuffix ")"

breakOn :: Text -> Text -> (Text, Text)
breakOn sep = second (T.drop $ T.length sep) . T.breakOn sep

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd sep = first (T.dropEnd $ T.length sep) . T.breakOnEnd sep

analyzeAnnotation :: AvailableImports -> Annotation -> [Solution]
analyzeAnnotation availableImports = \ case
  RedundantImport -> [RemoveImport]
  VariableNotInScope variable -> importName variable.qualification (Name VariableName variable.name)
  TermLevelUseOfTypeConstructor qualification name -> importName qualification (Name VariableName name)
  TypeNotInScope qualification name -> importName qualification (Name TypeName name)
  FoundHole _ fits -> map (UseName . (.name)) fits
  where
    ignore :: Module -> Bool
    ignore module_ = module_.name == "Test.Hspec.Discover"

    discardIgnored :: [ProvidedBy] -> [ProvidedBy]
    discardIgnored = filter $ providedByModule >>> (not . ignore)

    providedByModule :: ProvidedBy -> Module
    providedByModule (ProvidedBy module_ _) = module_

    importName :: Qualification -> Name -> [Solution]
    importName qualification required = map solution . sortByModule $ discardIgnored providedBy
      where
        sortByModule :: [ProvidedBy] -> [ProvidedBy]
        sortByModule = sortImports qualification required providedByModule

        providedBy :: [ProvidedBy]
        providedBy = fromMaybe [] (Map.lookup required availableImports)

        solution :: ProvidedBy -> Solution
        solution = \ case
          ProvidedBy module_ (Just (Type type_)) -> ImportName module_ qualification case required.nameSpace of
            VariableName -> type_ <> "(..)"
            TypeName -> type_
          ProvidedBy module_ Nothing -> ImportName module_ qualification required.name

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

addImport :: Builder -> [Text] -> [Text]
addImport statement input = case break (T.isPrefixOf "import ") input of
  (xs, ys) -> xs ++ Builder.toText statement : ys

importStatement :: Module -> Qualification -> [Text] -> Builder
importStatement (Module _ (Builder.fromText -> module_)) qualification names = case qualification of
  Unqualified -> "import " <> module_ <> " (" <> Builder.join ", " (map formatImportItem names) <> ")"
  Qualified name -> "import " <> module_ <> " qualified as " <> Builder.fromText name
  where
    formatImportItem :: Text -> Builder
    formatImportItem name = case T.uncons name of
      Just (c, _) | isLetter c || c == '_' -> Builder.fromText name
      _ -> "(" <> Builder.fromText name <> ")"

applyReplace :: Location -> Location -> Text -> [Text] -> [Text]
applyReplace start end substitute input =
  let
    (before, rest) = splitAt (start.line - 1) input

    after :: [Text]
    after = drop (end.line - start.line + 1) rest

  in case do
    firstLine <- head rest
    lastLine <- head $ drop (end.line - start.line) rest
    return $ mconcat [T.take (start.column - 1) firstLine, substitute, T.drop (end.column - 1) lastLine]
  of
    Nothing -> input
    Just substituted -> before ++ substituted : after

prependToFile :: FilePath -> Text -> IO ()
prependToFile file contents = do
  old <- Utf8.readFile file
  withFile file WriteMode $ \ h -> do
    Utf8.hPutStr h contents
    Utf8.hPutStr h old

modifyFile :: FilePath -> ([Text] -> [Text]) -> IO ()
modifyFile file f = do
  old <- T.lines <$> Utf8.readFile file
  Utf8.writeFile file . T.unlines $ f old
