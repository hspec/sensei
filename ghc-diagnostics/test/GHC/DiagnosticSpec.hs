{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module GHC.DiagnosticSpec (spec) where

import Helper

import System.IO
import System.IO.Temp (withSystemTempDirectory)
import Data.Ord (comparing)
import GHC.Fingerprint
import Test.Hspec.Expectations.Contrib qualified as Hspec
import Data.Yaml (Value)
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml
import Text.RawString.QQ (r, rQ)

import System.IO.Unsafe (unsafePerformIO)
import System.Process
import System.Directory
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.String.ANSI.Strip (stripAnsi)

import "ghc-hie" GHC.Iface.Ext.Binary
import GHC.Types.Name.Cache (initNameCache)

import GHC.HIE
import GHC.Diagnostic hiding (edits)
import GHC.Diagnostic qualified as Diagnostic
import GHC.Diagnostic.Annotated

shouldAnnotate :: Bool
shouldAnnotate = True

addAnnotation :: String -> IO a -> IO a
addAnnotation m = if shouldAnnotate then Hspec.annotate m else id

test, ftest, xtest :: HasCallStack => String -> [String] -> String -> Maybe Annotation -> [ExpectedSolution] -> Spec

test name args code = testWith name ANY args code

ftest name args code annotation = focus . test name args code annotation

xtest name args code annotation = before_ pending . test name args code annotation

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

normalizeGhcVersion :: String -> String
normalizeGhcVersion = unpack . T.replace __GLASGOW_HASKELL_FULL_VERSION__ "9.10.0" . T.pack

pretty :: String -> String
pretty input = case Yaml.decodeThrow @Maybe @Value $ encodeUtf8 input of
  Nothing -> input
  Just value -> decodeUtf8 (Yaml.encodePretty conf value)
  where
    conf = Yaml.setConfCompare (comparing f) Yaml.defConfig

    f :: Text -> Int
    f name = fromMaybe maxBound (lookup name fieldOrder)

fieldOrder :: [(Text, Int)]
fieldOrder = flip zip [1..] [
    "version"
  , "ghcVersion"
  , "span"
  , "severity"
  , "code"
  , "message"
  , "hints"
  , "reason"

  , "file"
  , "start"
  , "end"
  , "line"
  , "column"
  ]

data ExpectedSolution = ExpectedSolution {
  solution :: Solution
, withAppliedSolution :: FilePath -> Expectation
}

testWith :: HasCallStack => String -> GHC -> [String] -> String -> Maybe Annotation -> [ExpectedSolution] -> Spec
testWith name requiredVersion extraArgs (T.encodeUtf8 . unindent -> code) annotation solutions = it name do
  unless (B.null code) do
    ensureFile src code

  err <- translate <$> ghc []
  errNoContext <- translate <$> ghc ["-fno-show-error-context"]

  json <- pretty <$> ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 $ stripAnsi err)
  ensureFile (dir </> "err.yaml") (encodeUtf8 $ normalizeGhcVersion json)
  parseAnnotated getAvailableImports (encodeUtf8 json) >>= \ case
    Nothing -> do
      expectationFailure $ "Parsing JSON failed:\n\n" <> json
    Just annotated -> addAnnotation (separator <> json <> separator <> err <> separator) do
      whenGhc requiredVersion do
        format FormatConfig { showErrorContext = True, color = False } annotated.diagnostic `shouldBe` stripAnsi err
        format FormatConfig { showErrorContext = False, color = False } annotated.diagnostic `shouldBe` stripAnsi errNoContext
        format FormatConfig { showErrorContext = True, color = True } annotated.diagnostic `shouldBe` err
        format FormatConfig { showErrorContext = False, color = True } annotated.diagnostic `shouldBe` errNoContext

      annotated.annotation `shouldBe` annotation
      whenGhc requiredVersion do
        annotated.solutions `shouldBe` map (.solution) solutions
        let
          edits :: [Edit]
          edits = Diagnostic.edits annotated

          enumerate :: [a] -> [(Int, a)]
          enumerate = zip [1..]


        for_ (enumerate solutions) \ (n, solution) -> do
          withSystemTempDirectory "hspec" \ tmp -> do
            let
              dst :: FilePath
              dst = tmp </> src
            ensureFile dst code
            void $ tryJust (guard . isDoesNotExistError) do
              apply tmp (Just n) edits
            solution.withAppliedSolution dst
  where
    separator :: String
    separator = replicate 30 '*' <> "\n"

    dir :: FilePath
    dir = "test" </> "fixtures" </> name

    src :: FilePath
    src = dir </> "Foo.hs"

    ghc :: [String] -> IO String
    ghc args = do
      cached "ghc" (["-XNoStarIsType", "-fno-code", "-fno-diagnostics-show-caret", "-fdiagnostics-color=always", "-fprint-error-index-links=always"] ++ args ++ extraArgs ++ [src])

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      c -> c

cached :: FilePath -> [String] -> IO String
cached program args = do
  fingerprint <- combine program . fingerprintFingerprints <$> for args \ arg -> doesFileExist arg >>= \ case
    False -> return $ fingerprintString arg
    True -> combine arg <$> getFileHash arg

  cache <- getCacheDirectory

  let
    cacheFile :: String
    cacheFile = cache </> show fingerprint

    process :: CreateProcess
    process = proc program args

  doesFileExist cacheFile >>= \ case
    False -> do
      (_, _, err) <- readCreateProcessWithExitCode process ""
      bracket (openTempFile cache "sensei") (hClose . snd) \ (file, h) -> do
        hPutStr h err
        renameFile file cacheFile
      return err
    True -> do
      readFile cacheFile
  where
    combine :: String -> Fingerprint -> Fingerprint
    combine a b = fingerprintFingerprints [fingerprintString a, b]

getAvailableImports :: IO AvailableImports
getAvailableImports = do
  getAvailableImports_

{-# NOINLINE getAvailableImports_ #-}
getAvailableImports_ :: IO AvailableImports
getAvailableImports_ = unsafePerformIO do
  home <- getHomeDirectory
  files <- listHieFiles $ home </> ".local" </> "state" </> "ghc-hie-files" </> "ghc-9.12.2" </> "base"
  nameCache <- initNameCache 'r' []
  hieFiles <- map hie_file_result <$> mapM (readHieFile nameCache) files
  let
    base :: Package
    base = Package DirectDependency "base"

    exports :: [(Name, ProvidedBy)]
    exports = concatMap (hieExports base) hieFiles
  return . return . Map.fromListWith (++) $ map (fmap singleton) exports

unindent :: String -> Text
unindent (pack >>> T.dropWhileEnd isSpace >>> T.lines -> input) = go input
  where
    go :: [Text] -> Text
    go = map (T.drop $ indentation input) >>> T.unlines >>> T.dropWhile isSpace

    indentation :: [Text] -> Int
    indentation = dropEmptyLines >>> map (T.length . T.takeWhile isSpace) >>> minimum

    dropEmptyLines :: [Text] -> [Text]
    dropEmptyLines = filter (not . T.all isSpace)

redundantImport :: Maybe Annotation
redundantImport = Just RedundantImport

notInScope :: RequiredVariable -> Maybe Annotation
notInScope = Just . VariableNotInScope

foundHole :: Text -> Type -> [HoleFit] -> Maybe Annotation
foundHole name type_ = Just . FoundHole name type_

expectFileContent :: HasCallStack => String -> FilePath -> Expectation
expectFileContent expected file = Utf8.readFile file `shouldReturn` unindent expected

enableExtension :: HasCallStack => Text -> String -> ExpectedSolution
enableExtension name = ExpectedSolution (EnableExtension name) . expectFileContent

enableExtension_ :: Text -> ExpectedSolution
enableExtension_ name = ExpectedSolution (EnableExtension name) mempty

ignoreWarning :: HasCallStack => Text -> String -> ExpectedSolution
ignoreWarning name = ExpectedSolution (IgnoreWarning name) . expectFileContent

ignoreWarning_ :: Text -> ExpectedSolution
ignoreWarning_ name = ExpectedSolution (IgnoreWarning name) mempty

removeImport :: HasCallStack => String -> ExpectedSolution
removeImport = ExpectedSolution RemoveImport . expectFileContent

removeImport_ :: ExpectedSolution
removeImport_ = ExpectedSolution RemoveImport mempty

replaceImport :: HasCallStack => Text -> Text -> String -> ExpectedSolution
replaceImport old new = ExpectedSolution (ReplaceImport old new) . expectFileContent

replaceImport_ :: Text -> Text -> ExpectedSolution
replaceImport_ old new = ExpectedSolution (ReplaceImport old new) mempty

createModule :: HasCallStack => String -> FilePath -> Text -> String -> ExpectedSolution
createModule name path module_ expected = ExpectedSolution (CreateModule modulePath module_) \ file -> do
  Utf8.readFile (takeDirectory file </> path) `shouldReturn` unindent expected
  where
    modulePath :: FilePath
    modulePath = "test" </> "fixtures" </> name </> path

createModule_ :: String -> FilePath -> Text -> ExpectedSolution
createModule_ name path module_ = ExpectedSolution (CreateModule modulePath module_) mempty
  where
    modulePath :: FilePath
    modulePath = "test" </> "fixtures" </> name </> path

replaceName :: HasCallStack => Text -> Text -> String -> ExpectedSolution
replaceName old new = ExpectedSolution (ReplaceName old new) . expectFileContent

replaceName_ :: Text -> Text -> ExpectedSolution
replaceName_ old new = ExpectedSolution (ReplaceName old new) mempty

importName :: HasCallStack => Module -> Qualification -> Text -> String -> ExpectedSolution
importName module_ qualification name = ExpectedSolution (ImportName module_ qualification name) . expectFileContent

importName_ :: Module -> Text -> ExpectedSolution
importName_ module_ name = ExpectedSolution (ImportName module_ Unqualified name) mempty

addArgument :: HasCallStack => Text -> String -> ExpectedSolution
addArgument expression = ExpectedSolution (AddArgument expression) . expectFileContent

spec :: Spec
spec = do
  describe "format" do
    test "not-in-scope" [] [r|
      module Foo where
      foo = catMaybes
      |] (notInScope "catMaybes") [
        importName "Data.Maybe" Unqualified "catMaybes" [r|
      module Foo where
      import Data.Maybe (catMaybes)
      foo = catMaybes
      |]
      ]

    test "not-in-scope-qualified" [] [r|
      module Foo where
      foo = M.catMaybes
      |] (notInScope (RequiredVariable "M" "catMaybes" NoTypeSignature)) [
        importName "Data.Maybe" "M" "catMaybes" [r|
      module Foo where
      import Data.Maybe qualified as M
      foo = M.catMaybes
      |]
      ]

    test "not-in-scope-with-type" [] [r|
      module Foo where
      foo :: Int
      foo = bar "baz"
      |] (notInScope (RequiredVariable Unqualified "bar" "String -> Int")) [
      ]

    test "not-in-scope-perhaps-use" [] [r|
      module Foo where
      foo = filter_
      |] (notInScope "filter_") [
        replaceName "filter_" "filter" [r|
      module Foo where
      foo = filter
      |]
      ]

    test "not-in-scope-perhaps-use-one-of-these" [] [r|
      module Foo where
      foo = fold
      |] (notInScope "fold") [
        replaceName_ "fold" "foldl"
      , replaceName_ "fold" "foldr"
      , importName "Data.Foldable" Unqualified "Foldable(..)" [r|
      module Foo where
      import Data.Foldable (Foldable(..))
      foo = fold
      |]
      ]

    test "not-in-scope-perhaps-use-multiline" [] [r|
      module Foo where
      import Data.List
      foo = fold
      |] (notInScope "fold") [
        replaceName_ "fold" "foldl"
      , replaceName_ "fold" "foldr"
      , importName_ "Data.Foldable" "Foldable(..)"
      ]

    test "not-in-scope-operator" [] [r|
      module Foo where
      foo = (<&>)
      |] (notInScope (RequiredVariable Unqualified "<&>" NoTypeSignature)) [
        replaceName "<&>" "<>" [r|
      module Foo where
      foo = (<>)
      |]
      , replaceName_ "<&>" "<$>"
      , replaceName_ "<&>" "<*>"
      , importName "Data.Functor" Unqualified "<&>" [r|
      module Foo where
      import Data.Functor ((<&>))
      foo = (<&>)
      |]
      ]

    test "not-in-scope-operator-infix" [] [r|
      module Foo where
      foo :: [a] -> (Int, [a])
      foo = length &&& id
      |] (notInScope (RequiredVariable Unqualified "&&&" "(t0 a0 -> Int) -> (a1 -> a1) -> [a] -> (Int, [a])")) [
        replaceName "&&&" "&&" [r|
      module Foo where
      foo :: [a] -> (Int, [a])
      foo = length && id
      |]
      , importName_ "Control.Arrow" "Arrow(..)"
      ]

    test "not-in-scope-type" [] [r|
      module Foo where
      foo :: Map
      foo = undefined
      |] (Just $ TypeNotInScope Unqualified "Map") []

    test "not-in-scope-type-qualified" [] [r|
      module Foo where
      foo :: My.Map
      foo = undefined
      |] (Just $ TypeNotInScope "My" "Map") []

    test "not-in-scope-data" [] [r|
      module Foo where
      foo = NoArg
      |] (notInScope "NoArg") [
        importName_ "System.Console.GetOpt" "ArgDescr(..)"
      ]

    test "not-in-scope-data-with-type" [] [r|
      module Foo where

      foo :: Maybe Int
      foo = Foo (23 :: Int)
      |] (notInScope (RequiredVariable Unqualified "Foo" "Int -> Maybe Int")) [
        replaceName_ "Foo" "foo"
      ]

    test "not-in-scope-data-perhaps-use" [] [r|
      module Foo where
      data SomeOption = SomeOption
      foo = someOption
      |] (notInScope "someOption") [
        replaceName_ "someOption" "SomeOption"
      ]

    test "not-in-scope-pattern" [] [r|
      module Foo where
      foo = case undefined of
        NoArg -> undefined
      |] (notInScope "NoArg") [
        importName_ "System.Console.GetOpt" "ArgDescr(..)"
      ]

    test "term-level-use-of-type-constructor" [] [r|
      module Foo where
      data Foo = Fooa | Fooi
      foo = Foo
      |] (Just $ TermLevelUseOfTypeConstructor "Foo") [
        replaceName_ "Foo" "foo"
      , replaceName_ "Foo" "Fooa"
      , replaceName_ "Foo" "Fooi"
      ]

    test "found-hole" [] [r|
      module Foo where
      foo :: FilePath -> IO String
      foo name = do
        r <- _ name
        return r
      |] (foundHole "_" "FilePath -> IO String" [
        HoleFit "foo" "FilePath -> IO String"
      , HoleFit "readFile" "FilePath -> IO String"
      , HoleFit "readIO" "forall a. Read a => String -> IO a"
      , HoleFit "return" "forall (m :: Type -> Type) a. Monad m => a -> m a"
      , HoleFit "fail" "forall (m :: Type -> Type) a. MonadFail m => String -> m a"
      , HoleFit "pure" "forall (f :: Type -> Type) a. Applicative f => a -> f a"
      ]
      ) [
        replaceName_ "_" "foo"
      , replaceName_ "_" "readFile"
      , replaceName_ "_" "readIO"
      , replaceName_ "_" "return"
      , replaceName_ "_" "fail"
      , replaceName_ "_" "pure"
      ]

    test "found-hole-no-type" ["-fno-show-type-of-hole-fits"] [r|
      module Foo where
      foo :: FilePath -> IO String
      foo name = do
        r <- _ name
        return r
      |] (foundHole "_" "FilePath -> IO String" [
        HoleFit "foo" NoTypeSignature
      , HoleFit "readFile" NoTypeSignature
      , HoleFit "readIO" NoTypeSignature
      , HoleFit "return" NoTypeSignature
      , HoleFit "fail" NoTypeSignature
      , HoleFit "pure" NoTypeSignature
      ]
      ) [
        replaceName_ "_" "foo"
      , replaceName_ "_" "readFile"
      , replaceName_ "_" "readIO"
      , replaceName_ "_" "return"
      , replaceName_ "_" "fail"
      , replaceName_ "_" "pure"
      ]

    test "found-hole-single-line" [] [r|
      {-# LINE 1 "A" #-}
      data A
      a :: A
      a = _
      |] (foundHole "_" "A" [
        HoleFit "a" "A"
      ]
      ) [
        replaceName_ "_" "a"
      ]

    test "found-hole-named" [] [r|
      data A
      a :: A
      a = _foo
      |] (foundHole "_foo" "A" [
        HoleFit "a" "A"
      ]
      ) [
        replaceName_ "_foo" "a"
      ]

    test "found-hole-multiline-signature" [] [r|
      a :: String -> String -> String -> String -> String -> String -> String -> String
      a = _
      |] (foundHole "_" "String -> String -> String -> String -> String -> String -> String -> String" [
        HoleFit "a" "String -> String -> String -> String -> String -> String -> String -> String"
      , HoleFit "mempty" "forall a. Monoid a => a"
      ]
      ) [
        replaceName_ "_" "a"
      , replaceName_ "_" "mempty"
      ]

    test "found-type-hole" [] [r|
      module Foo where
      foo :: FilePath -> IO _
      foo = readFile
      |] (Just $ FoundTypeHole "_" "String") [
        replaceName_ "_" "String"
      , enableExtension_ "PartialTypeSignatures"
      ]

    test "found-type-hole-named" [] [r|
      module Foo where
      foo :: FilePath -> IO _foo
      foo = readFile
      |] (Just $ FoundTypeHole "_foo" "String") [
        replaceName_ "_foo" "String"
      , enableExtension_ "PartialTypeSignatures"
      ]

    test "too-few-arguments" [] [r|
      module Foo where
      foo :: Maybe Int
      foo = Just
      |] Nothing [
        addArgument "Just" [r|
      module Foo where
      foo :: Maybe Int
      foo = Just _
      |]
      ]

    test "use-BlockArguments" [] [r|
      {-# LANGUAGE NoBlockArguments #-}
      module Foo where

      foo :: IO ()
      foo = id do return ()
      |] Nothing [
        enableExtension_ "BlockArguments"
      ]

    test "use-TemplateHaskellQuotes" [] [rQ|
      module Foo where
      foo = [|23|~]
      |] Nothing [
        enableExtension "TemplateHaskellQuotes" [rQ|
      {-# LANGUAGE TemplateHaskellQuotes #-}
      module Foo where
      foo = [|23|~]
      |]
      , enableExtension_ "TemplateHaskell"
      ]

    testWith "redundant-import" GHC_912 ["-Wall"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport [
        removeImport [r|
      module Foo where
      |]
      , ignoreWarning_ "unused-imports"
      ]

    testWith "redundant-import-error" GHC_912 ["-Wall", "-Werror"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport [
        removeImport_
      , ignoreWarning_ "unused-imports"
      ]

    test "unknown-import" [] [r|
      module Foo where
      import Bar
      |] (Just $ UnknownImport "Bar" []) [
        createModule_ "unknown-import" "Bar.hs" "Bar"
      ]

    test "unknown-import-suggestion" [] [r|
      module Foo where
      import Syste.IO
      |] (Just $ UnknownImport "Syste.IO" [
        "System.IO"
      ]) [
        replaceImport "Syste.IO" "System.IO" [r|
      module Foo where
      import System.IO
      |]
      , createModule_ "unknown-import-suggestion" "Syste/IO.hs" "Syste.IO"
      ]

    test "unknown-import-multiline-suggestion" [] [r|
      module Foo where
      import Data.Binary.Gut
      |] (Just $ UnknownImport "Data.Binary.Gut" [
        "Data.Binary.Get"
      , "Data.Binary.Put"
      , "Data.Binary"
      ]) [
        replaceImport_ "Data.Binary.Gut" "Data.Binary.Get"
      , replaceImport_ "Data.Binary.Gut" "Data.Binary.Put"
      , replaceImport_ "Data.Binary.Gut" "Data.Binary"
      , createModule "unknown-import-multiline-suggestion" "Data/Binary/Gut.hs" "Data.Binary.Gut" [r|
      module Data.Binary.Gut where
      |]
      ]

    testWith "x-partial" GHC_912 [] [r|
      module Foo where
      foo = head
      |] Nothing [
        ignoreWarning "x-partial" [r|
      {-# OPTIONS_GHC -Wno-x-partial #-}
      module Foo where
      foo = head
      |]
      ]

    testWith "x-partial-error" GHC_912 ["-Werror"] [r|
      module Foo where
      foo = head
      |] Nothing [
        ignoreWarning_ "x-partial"
      ]

    test "non-existing" [] [r|
      |] Nothing []

    test "parse-error" [] [r|
      module Foo where

      data foo
      |] Nothing []

    test "lex-error" [] [r|
      module Foo where

      foo = "bar
      |] Nothing []

    test "multiple-error-messages" [] [r|
      module Foo where

      foo = "foo" + 23
      |] Nothing []

  describe "analyzeHint" do
    it "detects missing extension" do
      let
        inputs :: [Text]
        inputs = [
            requiredFor GHC_910 "Perhaps you intended to use BlockArguments"
          , requiredFor GHC_912 "Perhaps you intended to use the `BlockArguments' extension"
          ]
      for_ inputs \ input -> analyzeHint input `shouldBe` Just [
          EnableExtension "BlockArguments"
        ]

    it "detects missing extensions" do
      let
        inputs :: [Text]
        inputs = [
            requiredFor GHC_910 "Enable any of the following extensions: TemplateHaskell, TemplateHaskellQuotes"
          , requiredFor GHC_912 "Perhaps you intended to use the `TemplateHaskellQuotes' extension (implied by `TemplateHaskell')"
          ]
      for_ inputs \ input -> analyzeHint input `shouldBe` Just [
          EnableExtension "TemplateHaskellQuotes"
        , EnableExtension "TemplateHaskell"
        ]

  describe "extractIdentifiers" do
    it "extracts identifiers" do
      extractIdentifiers "foo" ".. `foldl' ..., `foldr' .." `shouldBe` [ReplaceName "foo" "foldl", ReplaceName "foo" "foldr"]

  describe "qualifiedName" do
    it "parses an unqualified name" do
      qualifiedName "foo" `shouldBe` RequiredVariable Unqualified "foo" NoTypeSignature

    it "parses a qualified name" do
      qualifiedName "Foo.Bar.baz" `shouldBe` RequiredVariable "Foo.Bar" "baz" NoTypeSignature

    it "parses a name with type signature" do
      qualifiedName "Foo.Bar.baz :: Int -> Int" `shouldBe` RequiredVariable "Foo.Bar" "baz" "Int -> Int"

  describe "formatAnnotated" do
    let
      solution :: Int -> String -> String
      solution n m = mconcat ["    [", fromString (show n), "] ", m]

      formatConfig :: FormatConfig
      formatConfig = FormatConfig { showErrorContext = False, color = True }

    it "formats an annotated diagnostic message" do
      Just annotated <- B.readFile "test/fixtures/not-in-scope/err.yaml" >>= parseAnnotated getAvailableImports
      stripAnsi . unpack <$> formatAnnotated formatConfig 1 annotated `shouldBe` (2, unlines [
          "test/fixtures/not-in-scope/Foo.hs:2:7: error: [GHC-88464]"
        , "    Variable not in scope: catMaybes"
        , ""
        , solution 1 "import Data.Maybe (catMaybes) (base)"
        , ""
        ])

    it "formats an annotated diagnostic message" do
      Just annotated <- B.readFile "test/fixtures/not-in-scope-operator/err.yaml" >>= parseAnnotated getAvailableImports
      stripAnsi . unpack <$> formatAnnotated formatConfig 1 annotated `shouldBe` (5, unlines [
          "test/fixtures/not-in-scope-operator/Foo.hs:2:7: error: [GHC-88464]"
        , "    Variable not in scope: <&>"
        , "    Suggested fix:"
        , "      Perhaps use one of these:"
        , "        `<>' (imported from Prelude), `<$>' (imported from Prelude),"
        , "        `<*>' (imported from Prelude)"
        , ""
        , solution 1 "Use <>"
        , solution 2 "Use <$>"
        , solution 3 "Use <*>"
        , solution 4 "import Data.Functor ((<&>)) (base)"
        , ""
        ])

  describe "analyzeAnnotation" do
    let
      diagnostic :: Diagnostic
      diagnostic = Diagnostic {
        version = "1.0"
      , ghcVersion = "ghc-9.10.0"
      , span = Nothing
      , severity = Error
      , code = Nothing
      , message = []
      , hints = []
      , reason = Nothing
      }

    beforeAll getAvailableImports do
      context "with VariableNotInScope" do
        it "suggests functions" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = VariableNotInScope (RequiredVariable Unqualified "getFileHash" NoTypeSignature)
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
              ImportName "GHC.Fingerprint" Unqualified "getFileHash"
            ]

        it "suggests class methods" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = VariableNotInScope (RequiredVariable Unqualified "show" NoTypeSignature)
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
              ImportName "Prelude" Unqualified "Show(..)"
            , ImportName "Text.Show" Unqualified "Show(..)"
            , ImportName "GHC.Show" Unqualified "Show(..)"
            ]

        it "suggests constructors" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = VariableNotInScope (RequiredVariable Unqualified "Option" NoTypeSignature)
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
              ImportName "System.Console.GetOpt" Unqualified "OptDescr(..)"
            ]

        it "does not suggest types" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = VariableNotInScope (RequiredVariable Unqualified "OptDescr" NoTypeSignature)
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
            ]

      context "with TypeNotInScope" do
        it "suggests types" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = TypeNotInScope Unqualified "OptDescr"
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
              ImportName "System.Console.GetOpt" Unqualified "OptDescr"
            ]

        it "does not suggest constructors" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = TypeNotInScope Unqualified "Option"
          analyzeAnnotation availableImports diagnostic annotation `shouldBe` [
            ]

  describe_ 'addImport do
    it "adds the import before the first import statement" do
      addImport "import Data.Text" (unindent [r|
      module Foo where

      -- some comment

      import Data.ByteString
      |]) `shouldBe` (unindent [r|
      module Foo where

      -- some comment

      import Data.Text
      import Data.ByteString
      |])

    context "without any import statement" do
      it "adds the import after the module header" do
        addImport "import Data.Text" (unindent [r|
        module Foo where

        -- some comment
        |]) `shouldBe` (unindent [r|
        module Foo where
        import Data.Text

        -- some comment
        |])

      context "without any module header" do
        it "adds the import at the start of the file" do
          addImport "import Data.Text" (unindent [r|
          -- some comment
          |]) `shouldBe` (unindent [r|
          import Data.Text
          -- some comment
          |])
