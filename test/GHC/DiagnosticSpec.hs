{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module GHC.DiagnosticSpec (spec) where

import Data.ByteString qualified as B
import           Helper hiding (diagnostic)
import           Test.Hspec.Expectations.Contrib qualified as Hspec
import           Text.RawString.QQ (r, rQ)

import           System.Process
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated
import qualified Data.Map as Map

data Requirement = NoRequirement | RequireGhc912

test, ftest, xtest :: HasCallStack => FilePath -> [String] -> String -> Maybe Annotation -> [Solution] -> Spec

test name = testWith name NoRequirement

ftest name args code annotation = focus . test name args code annotation

xtest name args code annotation = before_ pending . test name args code annotation

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

normalizeGhcVersion :: String -> String
normalizeGhcVersion = T.unpack . T.replace __GLASGOW_HASKELL_FULL_VERSION__ "9.10.0" . T.pack

availableImports :: AvailableImports
availableImports = Map.fromList [
    ("c2w", ["Data.ByteString.Internal"])
  , ("fromList", ["Data.Map"])
  ]


testWith :: HasCallStack => FilePath -> Requirement -> [String] -> String -> Maybe Annotation -> [Solution] -> Spec
testWith name requirement extraArgs (unindent -> code) action solutions = it name $ do
  unless (T.null code) do
    ensureFile src $ T.encodeUtf8 code
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") (encodeUtf8 $ normalizeGhcVersion json)
  Hspec.annotate (separator <> err <> separator) do
    Just annotated <- return . parseAnnotated availableImports $ encodeUtf8 json
    when shouldRun $ do
      format annotated.diagnostic `shouldBe` err
    annotated.annotation `shouldBe` action
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
      requireGhc [9,10]
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

    shouldRun :: Bool
    shouldRun = case requirement of
      NoRequirement -> True
      RequireGhc912 ->
#if __GLASGOW_HASKELL__ < 912
        False
#else
        True
#endif

unindent :: String -> Text
unindent (T.pack >>> T.dropWhileEnd isSpace >>> T.lines -> input) = go input
  where
    go :: [Text] -> Text
    go = map (T.drop $ indentation input) >>> T.unlines >>> T.dropWhile isSpace

    indentation :: [Text] -> Int
    indentation = dropEmptyLines >>> map (T.length . T.takeWhile isSpace) >>> maximum

    dropEmptyLines :: [Text] -> [Text]
    dropEmptyLines = filter (not . T.all isSpace)

variableNotInScope :: RequiredVariable -> Maybe Annotation
variableNotInScope name = Just $ NotInScope name

redundantImport :: Maybe Annotation
redundantImport = Just RedundantImport

suggestImport :: Module -> Text -> Solution
suggestImport module_ name = ImportName module_ Unqualified name

spec :: Spec
spec = do
  describe "format" $ do
    test "not-in-scope" [] [r|
      module Foo where
      foo = c2w
      |] (variableNotInScope "c2w") [suggestImport "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-type-signature" [] [r|
      module Foo where
      foo :: String -> String -> String -> String -> String -> String -> String
      foo = c2w
      |] (variableNotInScope $ RequiredVariable Unqualified "c2w" $ Just "String -> String -> String -> String -> String -> String -> String") [suggestImport "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-qualified" [] [r|
      module Foo where
      foo = B.c2w
      |] (variableNotInScope $ RequiredVariable "B" "c2w" Nothing) [ImportName "Data.ByteString.Internal" (Qualified "B") "c2w"]

    test "not-in-scope-qualified-2" [] [r|
      module Foo where
      import Data.List.NonEmpty qualified as Ma
      foo = Map.fromList
      |] (variableNotInScope $ RequiredVariable "Map" "fromList" Nothing) [
          UseName "Ma.fromList"
        , ImportName "Data.Map" (Qualified "Map") "fromList"
        ]

    test "not-in-scope-perhaps-use" [] [r|
      module Foo where
      foo = filter_
      |] (variableNotInScope "filter_") [UseName "filter"]

    test "not-in-scope-perhaps-use-one-of-these" [] [r|
      module Foo where
      foo = fold
      |] (variableNotInScope "fold") [UseName "foldl", UseName "foldr"]

    test "not-in-scope-perhaps-use-multiline" [] [r|
      module Foo where
      import Data.List
      foo = fold
      |] (variableNotInScope "fold") [UseName "foldl", UseName "foldr"]

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

    testWith "redundant-import" RequireGhc912 ["-Wall", "-Werror"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport [RemoveImport]

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

  describe "extractIdentifiers" $ do
    it "extracts identifiers" $ do
      extractIdentifiers ".. `foldl' ..., `foldr' .." `shouldBe` [UseName "foldl", UseName "foldr"]

  describe "qualifiedName" $ do
    it "" do
      qualifiedName "foo" `shouldBe` RequiredVariable Unqualified "foo" Nothing
      qualifiedName "Foo.Bar.baz" `shouldBe` RequiredVariable "Foo.Bar" "baz" Nothing

  describe "formatAnnotated" $ do
    it "extracts identifiers" $ do
      err <- B.readFile "test/fixtures/not-in-scope/err.json"
      Just foo <- return $ parseAnnotated availableImports err
      formatAnnotated foo `shouldBe` T.unlines [
          "test/fixtures/not-in-scope/Foo.hs:2:7: error: [GHC-88464]"
        , "    Variable not in scope: c2w"
        , ""
        , T.pack (withColor Cyan "    [1] ") <> "import Data.ByteString.Internal (c2w)"
        ]

  describe "applyReplace" $ do
    it "replaces a given source span with a substitute" $ do
      applyReplace (Location 2 7) (Location 2 14) "filter" [
          "module Foo where"
        , "foo = filter_ p xs"
        ] `shouldBe` [
          "module Foo where"
        , "foo = filter p xs"
        ]

    it "correctly handles source spans that span over multiple lines" $ do
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

        {-
src/Command.hs:263:106: error: [GHC-88464]
    Data constructor not in scope: PascalCase :: [String] -> String
    Suggested fix: Perhaps use variable `pascalCase' (line 366)

test/Language/Haskell/GhciWrapperSpec.hs:164:25: error: [GHC-88464]
    Data constructor not in scope:
      NotInScope :: t0 -> [a0] -> GHC.Diagnostic.Annotated.Annotation
    -}
