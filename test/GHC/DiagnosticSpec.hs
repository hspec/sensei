{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module GHC.DiagnosticSpec (spec) where

import           Helper hiding (diagnostic)
import           Test.Hspec.Expectations.Contrib qualified as Hspec
import           Text.RawString.QQ (r, rQ)

import           System.Process
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString qualified as B
import qualified Data.Map as Map

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated

test, ftest, xtest :: HasCallStack => FilePath -> [String] -> String -> Maybe Annotation -> [Solution] -> Spec

test name = testWith name minBound

ftest name args code annotation = focus . test name args code annotation

xtest name args code annotation = before_ pending . test name args code annotation

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

normalizeGhcVersion :: String -> String
normalizeGhcVersion = T.unpack . T.replace __GLASGOW_HASKELL_FULL_VERSION__ "9.10.0" . T.pack

testWith :: HasCallStack => FilePath -> GHC -> [String] -> String -> Maybe Annotation -> [Solution] -> Spec
testWith name requiredVersion extraArgs (unindent -> code) annotation solutions = it name do
  unless (T.null code) do
    ensureFile src $ T.encodeUtf8 code
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") (encodeUtf8 $ normalizeGhcVersion json)
  case parseAnnotated availableImports $ encodeUtf8 json of
    Nothing -> do
      expectationFailure $ "Parsing JSON failed:\n\n" <> json
    Just annotated -> Hspec.annotate (separator <> err <> separator) do
      whenGhc requiredVersion do
        format annotated.diagnostic `shouldBe` err
      annotated.annotation `shouldBe` annotation
      annotated.solutions `shouldBe` solutions
  where
    separator :: String
    separator = replicate 30 '*' <> "\n"

    dir :: FilePath
    dir = "test" </> "fixtures" </> name

    src :: FilePath
    src = dir </> "Foo.hs"

    ghc :: [String] -> IO String
    ghc args = do
      require GHC_910
      bin <- lookupGhc <$> getEnvironment
      let
        process :: CreateProcess
        process = proc bin ("-fno-code" : args ++ extraArgs ++ [src])
      (_, _, err) <- readCreateProcessWithExitCode process ""
      return err

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      c -> c

availableImports :: AvailableImports
availableImports = Map.fromList [
    ("c2w", ["Data.ByteString.Internal"])
  , ("fromList", ["Data.Map"])
  ]

unindent :: String -> Text
unindent (T.pack >>> T.dropWhileEnd isSpace >>> T.lines -> input) = go input
  where
    go :: [Text] -> Text
    go = map (T.drop $ indentation input) >>> T.unlines >>> T.dropWhile isSpace

    indentation :: [Text] -> Int
    indentation = dropEmptyLines >>> map (T.length . T.takeWhile isSpace) >>> maximum

    dropEmptyLines :: [Text] -> [Text]
    dropEmptyLines = filter (not . T.all isSpace)

redundantImport :: Maybe Annotation
redundantImport = Just RedundantImport

notInScope :: RequiredVariable -> Maybe Annotation
notInScope = Just . NotInScope

importName :: Module -> Text -> Solution
importName module_ = ImportName module_ Unqualified

spec :: Spec
spec = do
  describe "format" do
    test "not-in-scope" [] [r|
      module Foo where
      foo = c2w
      |] (notInScope "c2w") [importName "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-perhaps-use" [] [r|
      module Foo where
      foo = filter_
      |] (notInScope "filter_") [UseName "filter"]

    test "not-in-scope-perhaps-use-one-of-these" [] [r|
      module Foo where
      foo = fold
      |] (notInScope "fold") [UseName "foldl", UseName "foldr"]

    test "not-in-scope-perhaps-use-multiline" [] [r|
      module Foo where
      import Data.List
      foo = fold
      |] (notInScope "fold") [UseName "foldl", UseName "foldr"]

    test "use-BlockArguments" [] [r|
      {-# LANGUAGE NoBlockArguments #-}
      module Foo where

      foo :: IO ()
      foo = id do return ()
      |] Nothing [EnableExtension "BlockArguments"]

    test "use-TemplateHaskellQuotes" [] [rQ|
      module Foo where
      foo = [|23|~]
      |] Nothing [EnableExtension "TemplateHaskellQuotes", EnableExtension "TemplateHaskell"]

    testWith "redundant-import" GHC_912 ["-Wall"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport [RemoveImport]

    testWith "redundant-import-error" GHC_912 ["-Wall", "-Werror"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport [RemoveImport]

    testWith "x-partial" GHC_912 [] [r|
      module Foo where
      foo = head
      |] Nothing []

    testWith "x-partial-error" GHC_912 ["-Werror"] [r|
      module Foo where
      foo = head
      |] Nothing []

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
      extractIdentifiers ".. `foldl' ..., `foldr' .." `shouldBe` [UseName "foldl", UseName "foldr"]

  describe "qualifiedName" do
    it "parses an unqualified name" do
      qualifiedName "foo" `shouldBe` RequiredVariable Unqualified "foo" Nothing

    it "parses a qualified name" do
      qualifiedName "Foo.Bar.baz" `shouldBe` RequiredVariable "Foo.Bar" "baz" Nothing

  describe "formatAnnotated" do
    it "formats an annotated diagnostic message" do
      Just annotated <- B.readFile "test/fixtures/not-in-scope/err.json" <&> parseAnnotated availableImports
      formatAnnotated annotated `shouldBe` T.unlines [
          "test/fixtures/not-in-scope/Foo.hs:2:7: error: [GHC-88464]"
        , "    Variable not in scope: c2w"
        , ""
        , T.pack (withColor Cyan "    [1] ") <> "import Data.ByteString.Internal (c2w)"
        ]

  describe "applyReplace" do
    it "replaces a given source span with a substitute" do
      applyReplace (Location 2 7) (Location 2 14) "filter" [
          "module Foo where"
        , "foo = filter_ p xs"
        ] `shouldBe` [
          "module Foo where"
        , "foo = filter p xs"
        ]

    it "correctly handles source spans that span over multiple lines" do
      applyReplace (Location 2 8) (Location 3 7) "Ya" [
          "module Foo where"
        , "import Data.Maybe"
        , "foo = bar"
        , "one = two"
        ] `shouldBe` [
          "module Foo where"
        , "import Yabar"
        , "one = two"
        ]
