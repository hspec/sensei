{-# OPTIONS_GHC -Wno-unused-do-bind #-}
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
, applyAll

#ifdef TEST
, analyzeHint
, qualifiedName
, analyzeAnnotation
, addImport
#endif
) where

import           Imports hiding (stripPrefix, takeExtensions)
import           Builder (Builder)
import qualified Builder
import           System.Console.ANSI.Types

import           System.IO
import           System.Directory (createDirectoryIfMissing)
import qualified Data.List as List
import           Data.Text (stripPrefix, stripSuffix)
import qualified Data.Text as T hiding (stripPrefix, stripSuffix)
import qualified Data.Text.IO.Utf8 as Utf8
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import           GHC.Diagnostic.Type as Diagnostic
import           GHC.Diagnostic.Annotated
import           GHC.Diagnostic.Util
import qualified GHC.Diagnostic.Edit as Edit

run :: Parser a -> Text -> Maybe a
run p = either (\ _ -> Nothing) Just . A.parseOnly p

quoted :: Parser Text
quoted = unicode <|> requiredFor GHC_912 ascii
  where
    unicode = "\8216" *> A.takeTill (== '\8217') <* "\8217"
    ascii = "`" *> A.takeTill (== '\'') <* "'"

typeName :: Parser Text
typeName = T.unwords . T.words <$> quoted

skipTillQuoted :: Parser ()
skipTillQuoted = A.skipWhile \ c ->
     c /= '\8216'
  && requiredFor GHC_912 (c /= '`')

remainingInput :: Parser Text
remainingInput = A.takeText

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
      IgnoreWarning warning -> "Ignore warning: " <> Builder.fromText warning
      RemoveImport -> "Remove import"
      ReplaceImport _ new -> "Use " <> Builder.fromText new
      CreateModule file _ -> "Create " <> Builder.fromString file
      ReplaceName _ name -> "Use " <> Builder.fromText name
      ImportName module_ qualification name -> importStatement module_ qualification [name] <> faint package
        where
          package = " (" <> Builder.fromText module_.package.name <> ")"
      AddArgumentTo _ -> "Insert hole: _"
      AddPatterns _ -> "Add missing patterns"
      AddFields _ -> "Add missing fields"
      DeriveInstance text -> "derive instance " <> Builder.fromText text

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
  availableImports -> return Annotated { diagnostic, annotation, solutions }
    where
      annotation :: Maybe Annotation
      annotation = parseAnnotation diagnostic

      solutions :: [Solution]
      solutions =
           analyzeMessageContext
        ++ analyzeHints diagnostic.hints
        ++ maybe [] (analyzeAnnotation availableImports diagnostic) annotation
        ++ analyzeReason

      analyzeMessageContext :: [Solution]
      analyzeMessageContext = mapMaybe (run missingArgument) messageLines
        where
          messageLines :: [Text]
          messageLines = concatMap T.lines diagnostic.message

          missingArgument :: Parser Solution
          missingArgument = "Probable cause: " *> name <* " is applied to too few arguments"
            where
              name :: Parser Solution
              name = AddArgumentTo <$> quoted

      analyzeReason :: [Solution]
      analyzeReason = case diagnostic.reason of
        Nothing -> []
        Just (ReasonCategory (Category name)) -> [IgnoreWarning $ pack name]
        Just (ReasonFlags (Flags names)) -> map (IgnoreWarning . pack) names

analyzeHints :: [Text] -> [Solution]
analyzeHints = concat . mapMaybe analyzeHint

analyzeHint :: Text -> Maybe [Solution]
analyzeHint hint = asum [
    prefix "Perhaps you intended to use " <&> takeExtensions

  , requiredFor GHC_910 $ prefix "Enable any of the following extensions: " <&>
      map EnableExtension . reverse . T.splitOn ", "
  ]
  where
    prefix :: Text -> Maybe Text
    prefix p = stripPrefix p hint

    takeExtensions :: Text -> [Solution]
    takeExtensions input = fromMaybe takeExtensionGhc910 $ run takeExtensionGhc912 input
      where
        takeExtensionGhc910 :: [Solution]
        takeExtensionGhc910 = requiredFor GHC_910 [EnableExtension input]

        takeExtensionGhc912 :: Parser [Solution]
        takeExtensionGhc912 = (:) <$> extension <*> opt impliedBy
          where
            name :: Parser Solution
            name = EnableExtension <$> quoted

            extension :: Parser Solution
            extension = "the " *> name <* " extension"

            impliedBy :: Parser Solution
            impliedBy = " (implied by " *> name <* ")"

            opt :: Parser a -> Parser [a]
            opt p = A.option [] $ return <$> p

analyzePerhapsUseHints :: Text -> [Text] -> [Solution]
analyzePerhapsUseHints old = concat . mapMaybe (analyzePerhapsUseHint old)

analyzePerhapsUseHint :: Text -> Text -> Maybe [Solution]
analyzePerhapsUseHint old = run $ asum [
    "Perhaps use " *> identifier_
  , "Perhaps use data constructor " *> identifier_
  , "Perhaps use variable " *> identifier_
  , "Perhaps use one of these:" *> extractIdentifiers
  ]
  where
    identifier_ :: Parser [Solution]
    identifier_ = return <$> identifier

    identifier :: Parser Solution
    identifier = ReplaceName old <$> quoted

    extractIdentifiers :: Parser [Solution]
    extractIdentifiers = A.many1 extractIdentifier

    extractIdentifier :: Parser Solution
    extractIdentifier = skipTillQuoted *> identifier

parseAnnotation :: Diagnostic -> Maybe Annotation
parseAnnotation diagnostic = case diagnostic.code of
  Just 20125 -> missingFields
  Just 95909 -> missingStrictFields
  Just 39999 -> firstMessage >>= run missingInstance
  Just 66111 -> Just RedundantImport
  Just 61948 -> firstMessage >>= run parseUnknownImport
  Just 87110 -> firstMessage >>= run parseUnknownImport
  Just 62161 -> firstMessage >>= run parseNonExhaustivePatternMatch
  _ -> analyzeMessage
  where
    firstMessage :: Maybe Text
    firstMessage = head diagnostic.message

    missingFields :: Maybe Annotation
    missingFields = firstMessage >>= dropFirstLine >>= mapM extractField <&> MissingFields
      where
        dropFirstLine :: Text -> Maybe [Text]
        dropFirstLine = fmap (T.lines >>> drop 1) . stripPrefix "Fields of "

    missingStrictFields :: Maybe Annotation
    missingStrictFields = firstMessage >>= dropFirstLine >>= mapM extractField <&> MissingFields
      where
        dropFirstLine :: Text -> Maybe [Text]
        dropFirstLine = fmap (T.lines >>> drop 1) . stripPrefix "Constructor "

    extractField :: Text -> Maybe Text
    extractField = stripPrefix "  " >=> stripSuffix " ::" . T.dropWhileEnd (/= ':')

    missingInstance :: Parser Annotation
    missingInstance = "No instance for " *> typeName <&> MissingInstance

    parseNonExhaustivePatternMatch :: Parser Annotation
    parseNonExhaustivePatternMatch = do
      "Pattern match(es) are non-exhaustive\n"
      "In a " *> optional "\\" *> "case alternative:\n"
      NonExhaustivePatternMatch <$> type_ <*> patterns
      where
        type_ :: Parser Text
        type_ = "    Patterns of type " *> typeName <* " not matched:" <* optional "\n"

        patterns :: Parser [Text]
        patterns = remainingInput <&> filter (/= "...") . map T.strip . T.lines

    parseUnknownImport :: Parser Annotation
    parseUnknownImport = UnknownImport <$> name <*> suggestions
      where
        name :: Parser Text
        name = "Could not find module " *> quoted <* ".\n"

        suggestions :: Parser [Text]
        suggestions = A.option [] do
          "Perhaps you meant"
          (:) <$> (A.skipSpace *> suggestion) <*> many ("\n  " >> suggestion)

        suggestion :: Parser Text
        suggestion = A.takeWhile1 (not . isSpace) <* A.skipWhile (/= '\n')

    analyzeMessage :: Maybe Annotation
    analyzeMessage = asum . map analyzeMessageLine $ concatMap T.lines message
      where
        message :: [Text]
        message = map joinMessageLines diagnostic.message

    analyzeMessageLine :: Text -> Maybe Annotation
    analyzeMessageLine input = asum [
        VariableNotInScope <$> variableNotInScope
      , VariableNotInScope <$> run qualifiedNameNotInScope input
      , VariableNotInScope <$> dataConstructorNotInScope
      , VariableNotInScope <$> run dataConstructorNotInScopeInPattern input
      , TermLevelUseOfTypeConstructor <$> run termLevelUseOfTypeConstructor input
      , run typeConstructorNotInScope input
      , foundHole
      , run foundTypeHole input
      ]
      where
        foundHole :: Maybe Annotation
        foundHole = do
          (name, signature) <- prefix "Found hole: " <&> T.span (not . isSpace)
          FoundHole name <$> takeTypeSignature signature <*> analyzeHoleFits

        foundTypeHole :: Parser Annotation
        foundTypeHole = FoundTypeHole <$> ("Found type wildcard " *> quoted) <* " standing for " <*> quoted

        prefix :: Text -> Maybe Text
        prefix p = stripPrefix p input

        variableNotInScope :: Maybe RequiredVariable
        variableNotInScope = prefix "Variable not in scope: " <&> qualifiedName

        qualifiedNameNotInScope :: Parser RequiredVariable
        qualifiedNameNotInScope = "Not in scope: " *> quotedQualifiedName

        dataConstructorNotInScope :: Maybe RequiredVariable
        dataConstructorNotInScope = prefix "Data constructor not in scope: " <&> qualifiedName

        dataConstructorNotInScopeInPattern :: Parser RequiredVariable
        dataConstructorNotInScopeInPattern = "Not in scope: data constructor " *> quotedQualifiedName

        termLevelUseOfTypeConstructor :: Parser RequiredVariable
        termLevelUseOfTypeConstructor = asum [
            "Data constructor out of scope: "
          , requiredFor GHC_912 "Illegal term-level use of the type constructor "
          ] *> quotedQualifiedName

        typeConstructorNotInScope :: Parser Annotation
        typeConstructorNotInScope = "Not in scope: type constructor or class " *> (qualified TypeNotInScope <$> quoted)

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

quotedQualifiedName :: Parser RequiredVariable
quotedQualifiedName = qualifiedName <$> quoted

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

analyzeAnnotation :: AvailableImports -> Diagnostic -> Annotation -> [Solution]
analyzeAnnotation availableImports diagnostic = \ case
  RedundantImport -> [RemoveImport]
  UnknownImport name suggestions -> map (ReplaceImport name) suggestions ++ createModule name
  VariableNotInScope variable -> variableNotInScope variable diagnostic.hints
  TermLevelUseOfTypeConstructor variable -> variableNotInScope variable (requiredFor GHC_912 diagnostic.message ++ diagnostic.hints)
  TypeNotInScope qualification name -> typeNotInScope qualification name diagnostic.hints
  FoundHole name _ fits -> map (ReplaceName name . (.name)) fits
  FoundTypeHole name type_ -> [ReplaceName name type_, EnableExtension "PartialTypeSignatures"]
  NonExhaustivePatternMatch _name patterns -> [AddPatterns patterns]
  MissingFields fields -> [AddFields fields]
  MissingInstance text -> [DeriveInstance text]
  where
    createModule :: Text -> [Solution]
    createModule name = case diagnostic.span <&> (.file) <&> List.takeWhile (not . isUpper) of
      Nothing -> []
      Just dir -> [CreateModule file name]
        where
          file = dir </> unpack (T.replace "." "/" name) <> ".hs"

    ignore :: Module -> Bool
    ignore module_ = module_.name == "Test.Hspec.Discover"

    discardIgnored :: [ProvidedBy] -> [ProvidedBy]
    discardIgnored = filter $ providedByModule >>> (not . ignore)

    providedByModule :: ProvidedBy -> Module
    providedByModule (ProvidedBy module_ _) = module_

    variableNotInScope :: RequiredVariable -> [Text] -> [Solution]
    variableNotInScope variable hints = useSuggestedNames variable.qualification variable.name hints ++ importVariable variable

    typeNotInScope :: Qualification -> Text -> [Text] -> [Solution]
    typeNotInScope qualification name hints = useSuggestedNames qualification name hints ++ importName qualification (Name TypeName name)

    useSuggestedNames :: Qualification -> Text -> [Text] -> [Solution]
    useSuggestedNames qualification name = analyzePerhapsUseHints case qualification of
      Unqualified -> name
      Qualified q -> T.concat [q, ".", name]

    importVariable :: RequiredVariable -> [Solution]
    importVariable variable = importName variable.qualification (Name VariableName variable.name)

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
  | AddGhcOption FilePath Text
  | AddImport FilePath Module Qualification [Text]
  | Replace Span Text
  | ReplaceFirst Span Text Text
  | Append FilePath Text
  | CreateFile FilePath Text
  deriving (Eq, Show)

edits :: Annotated -> [Edit]
edits annotated = case annotated.diagnostic.span of
  Nothing -> []
  Just span -> map toEdit annotated.solutions
    where
      toEdit :: Solution -> Edit
      toEdit = \ case
        EnableExtension name -> AddExtension file name
        IgnoreWarning warning -> AddGhcOption file $ "-Wno-" <> warning
        RemoveImport -> removeLines
        ReplaceImport old new -> ReplaceFirst span old new
        CreateModule dst name -> CreateFile dst $ T.concat ["module ", name, " where\n"]
        ReplaceName old new -> ReplaceFirst span old new
        ImportName module_ qualification name -> AddImport file module_ qualification [name]
        AddArgumentTo _ -> insertEnd " _"
        AddPatterns patterns -> insertEnd . T.intercalate "\n" $ "" : map formatMissingPattern patterns
        AddFields fields -> insertEndMinusOne . T.unlines $ "" : map formatMissingField fields
        DeriveInstance text -> Append file $ "\nderiving instance " <> text <> "\n"

      file :: FilePath
      file = span.file

      insertEnd :: Text -> Edit
      insertEnd = insertAt span.end

      insertEndMinusOne :: Text -> Edit
      insertEndMinusOne = insertAt span.end { column = span.end.column - 1 }

      insertAt :: Location -> Text -> Edit
      insertAt p = Replace (Span file p p)

      removeLines :: Edit
      removeLines = Replace diagnosticLines ""

      diagnosticLines :: Span
      diagnosticLines = span {
        start = Location span.start.line 1
      , end = Location (span.end.line + 1) 1
      }

      formatMissingPattern :: Text -> Text
      formatMissingPattern p = "  " <> p <> " -> undefined"

      formatMissingField :: Text -> Text
      formatMissingField name = ", " <> name <> " = undefined"

apply :: FilePath -> Maybe Int -> [Edit] -> IO ()
apply dir choice = selectChoice >>> applyChoice
  where
    selectChoice :: [Edit] -> Maybe Edit
    selectChoice = drop (maybe 0 pred choice) >>> head

    applyChoice :: Maybe Edit -> IO ()
    applyChoice = maybe pass (relativeTo dir >>> applyEdit)

applyAll :: FilePath -> [Edit] -> IO ()
applyAll dir = mapM_ (relativeTo dir >>> applyEdit) . nub

relativeTo :: FilePath -> Edit -> Edit
relativeTo dir = \ case
  AddExtension file name -> AddExtension (dir </> file) name
  AddGhcOption file name -> AddGhcOption (dir </> file) name
  AddImport file module_ qualification names -> AddImport (dir </> file) module_ qualification names
  Replace span substitute -> Replace span { file = dir </> span.file } substitute
  ReplaceFirst span old new -> ReplaceFirst span { file = dir </> span.file } old new
  Append file content -> Append (dir </> file) content
  CreateFile file content -> CreateFile (dir </> file) content

applyEdit :: Edit -> IO ()
applyEdit = \ case
  AddExtension file name -> do
    prependToFile file $ "{-# LANGUAGE " <> name <> " #-}\n"

  AddGhcOption file flag -> do
    prependToFile file $ "{-# OPTIONS_GHC " <> flag <> " #-}\n"

  AddImport file module_ qualification names -> do
    modifyFile file $ addImport (Builder.toText $ importStatement module_ qualification names)

  Replace span substitute -> do
    modifyFile span.file $ applyReplace span.start span.end substitute

  ReplaceFirst span old new -> do
    modifyFile span.file $ applyReplaceFirst span.start span.end old new

  Append file content -> do
    Utf8.appendFile file content

  CreateFile file content -> do
    createDirectoryIfMissing True (takeDirectory file)
    Utf8.writeFile file content

addImport :: Text -> Text -> Text
addImport statement (T.lines -> input) = T.unlines case break (T.isPrefixOf "import ") input of
  (_, []) -> case break (T.isSuffixOf " where") input of
    (body, []) -> statement : body
    (header, where_ : body) -> header ++ where_ : statement : body
  (header, body) -> header ++ statement : body

importStatement :: Module -> Qualification -> [Text] -> Builder
importStatement (Module _ (Builder.fromText -> module_)) qualification names = case qualification of
  Unqualified -> "import " <> module_ <> " (" <> Builder.join ", " (map formatImportItem names) <> ")"
  Qualified name -> "import " <> module_ <> " qualified as " <> Builder.fromText name
  where
    formatImportItem :: Text -> Builder
    formatImportItem name = case T.uncons name of
      Just (c, _) | isLetter c || c == '_' -> Builder.fromText name
      _ -> "(" <> Builder.fromText name <> ")"

applyReplace :: Location -> Location -> Text -> Text -> Text
applyReplace start end new input = case Edit.cut start end input of
  (prefix, _, suffix) -> mconcat [prefix, new, suffix]

applyReplaceFirst :: Location -> Location -> Text -> Text -> Text -> Text
applyReplaceFirst start end old new input = case Edit.cut start end input of
  (prefix, focus, suffix) -> mconcat [prefix, Edit.replaceFirst old new focus, suffix]

prependToFile :: FilePath -> Text -> IO ()
prependToFile file contents = do
  old <- Utf8.readFile file
  withFile file WriteMode $ \ h -> do
    Utf8.hPutStr h contents
    Utf8.hPutStr h old

modifyFile :: FilePath -> (Text -> Text) -> IO ()
modifyFile file f = Utf8.readFile file >>= Utf8.writeFile file . f
