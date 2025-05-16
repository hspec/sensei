{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
module GHC.DiagnosticSpec (spec) where

import Text.RawString.QQ (r, rQ)
import           Prelude hiding (span)

import           Helper hiding (diagnostic)

import           System.Process
import           System.Environment
import qualified Data.Text as Text

import           Language.Haskell.GhciWrapper (lookupGhc)
import           GHC.Diagnostic
import           GHC.Diagnostic.Annotated
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Hspec.Expectations.Contrib as Hspec

data Requirement = NoRequirement | RequireGhc912

test :: HasCallStack => FilePath -> [String] -> String -> Maybe Annotation -> Spec
test name = testWith name NoRequirement

normalizeGhcVersion :: String -> String
normalizeGhcVersion = Text.unpack . Text.replace __GLASGOW_HASKELL_FULL_VERSION__ "9.10.0" . Text.pack

testWith :: HasCallStack => FilePath -> Requirement -> [String] -> String -> Maybe Annotation -> Spec
testWith name requirement extraArgs (unindent -> code) action = it name $ do
  unless (T.null code) do
    ensureFile src $ T.encodeUtf8 code
  err <- translate <$> ghc ["-fno-diagnostics-show-caret"]
  json <- ghc ["-fdiagnostics-as-json", "--interactive", "-ignore-dot-ghci"]
  ensureFile (dir </> "err.out") (encodeUtf8 err)
  ensureFile (dir </> "err.json") (encodeUtf8 $ normalizeGhcVersion json)
  Hspec.annotate (separator <> err <> separator) do
    Just diagnostic <- return . parse $ encodeUtf8 json
    when shouldRun $ do
      format diagnostic `shouldBe` err
    (annotate identifierMap diagnostic).annotation `shouldBe` action
  where
    identifierMap :: IdentifierMap
    identifierMap = Map.fromList [
        ("c2w", [Identifier "Data.ByteString.Internal" "c2w"])
      , ("fromList", [Identifier "Data.Map" "fromList"])
      ]

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

-- normalizeFileName :: Action -> Action
-- normalizeFileName = \ case
--   Choices choices -> Choices $ map normalizeFileName choices
--   AddExtension _ name -> AddExtension "Foo.hs" name
--   Replace span substitute -> Replace span {file = "Foo.hs"} substitute

ftest :: HasCallStack => FilePath -> [String] -> String -> Maybe Annotation -> Spec
ftest name args code = focus . test name args code

xtest :: HasCallStack => FilePath -> [String] -> String -> Maybe Annotation -> Spec
xtest name args code = before_ pending . test name args code

_ignore :: ()
_ignore = let _ = (ftest, xtest) in ()

-- replace :: Location -> Location -> Text -> Action
-- replace start end = Replace (Span "Foo.hs" start end)
--
-- replace_ :: Location -> Location -> Text -> Maybe Action
-- replace_ start end = Just . replace start end
--
-- addExtension :: Text -> Maybe Action
-- addExtension = Just . AddExtension "Foo.hs"

-- foo = c2w


unindent :: String -> Text
unindent foo = T.dropWhile isSpace . T.unlines $ map (T.drop indentation) input
  where
    input :: [Text]
    input = T.pack >>> T.dropWhileEnd isSpace >>> T.lines $ foo

    indentation :: Int
    indentation = maximum $ map (T.length . T.takeWhile isSpace) $ filter (not . T.all isSpace) input

suggestIdentifier :: Module -> String -> SuggestIdentifier
suggestIdentifier module_ name = SuggestIdentifier $ Identifier module_ name

spec :: Spec
spec = focus do
  describe "format" $ do
    test "not-in-scope" [] [r|
      module Foo where
      foo = c2w
      |] $ variableNotInScope "c2w" [suggestIdentifier "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-type-signature" [] [r|
      module Foo where
      foo :: String -> String -> String -> String -> String -> String -> String
      foo = c2w
      |] $ variableNotInScope (RequiredVariable Unqualified "c2w" $ Just "String -> String -> String -> String -> String -> String -> String") [suggestIdentifier "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-qualified" [] [r|
      module Foo where
      foo = B.c2w
      |] $ variableNotInScope (RequiredVariable "B" "c2w" Nothing) [suggestIdentifier "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-qualified-2" [] [r|
      module Foo where
      import Data.List.NonEmpty qualified as Ma
      foo = Map.fromList
      |] $ variableNotInScope (RequiredVariable "Map" "fromList" Nothing) [
          IdentifierInScope "Ma.fromList"
        , suggestIdentifier "Data.Map" "fromList"
        ]

    test "not-in-scope-perhaps-use" [] [r|
      module Foo where
      foo = filter_
      |] $ variableNotInScope "filter_" [IdentifierInScope "filter"]

    test "not-in-scope-perhaps-use-one-of-these" [] [r|
      module Foo where
      foo = fold
      |] $ variableNotInScope "fold" [IdentifierInScope "foldl", IdentifierInScope "foldr"]

    test "not-in-scope-perhaps-use-multiline" [] [r|
      module Foo where
      import Data.List
      foo = fold
      |] $ variableNotInScope "fold" [IdentifierInScope "foldl", IdentifierInScope "foldr"]

    test "use-BlockArguments" [] [r|
      {-# LANGUAGE NoBlockArguments #-}
      module Foo where

      foo :: IO ()
      foo = id do return ()
      |] $ missingExtension ["BlockArguments"]

    test "use-TemplateHaskellQuotes" [] [rQ|
      module Foo where
      foo = [|23|~]
      |] $ missingExtension ["TemplateHaskell", "TemplateHaskellQuotes"]

    testWith "redundant-import" RequireGhc912 ["-Wall", "-Werror"] [r|
      module Foo where
      import Data.Maybe
      |] redundantImport

    test "non-existing" [] [r|
      |] Nothing

    test "parse-error" [] [r|
      module Foo where

      data foo
      |] Nothing

    test "lex-error" [] [r|
      module Foo where

      foo = "bar
      |] Nothing

    test "multiple-error-messages" [] [r|
      module Foo where

      foo = "foo" + 23
      |] Nothing

  describe "sortSuggestions" $ do
    it "" $ do
      let
        name :: RequiredVariable
        name = "fromList"
      sortSuggestions name [
          Identifier "Data.List.NonEmpty" "fromList"
        , Identifier "Data.Map" "fromList"
        , Identifier "Data.Map.Internal" "fromList"
        , Identifier "Data.Map.Lazy" "fromList"
        , Identifier "Data.Map.Strict" "fromList"
        , Identifier "Data.Map.Strict.Internal" "fromList"
        , Identifier "Data.Set" "fromList"
        ] `shouldBe` [
          Identifier "Data.List.NonEmpty" "fromList"
        , Identifier "Data.Map" "fromList"
        , Identifier "Data.Map.Internal" "fromList"
        , Identifier "Data.Map.Lazy" "fromList"
        , Identifier "Data.Map.Strict" "fromList"
        , Identifier "Data.Map.Strict.Internal" "fromList"
        , Identifier "Data.Set" "fromList"
        ]

    it "" $ do
      let
        name :: RequiredVariable
        name = RequiredVariable (Qualified "Map") "fromList" Nothing
      sortSuggestions name [
          Identifier "Data.List.NonEmpty" "fromList"
        , Identifier "Data.Map" "fromList"
        , Identifier "Data.Map.Internal" "fromList"
        , Identifier "Data.Map.Lazy" "fromList"
        , Identifier "Data.Map.Strict" "fromList"
        , Identifier "Data.Map.Strict.Internal" "fromList"
        , Identifier "Data.Set" "fromList"
        ] `shouldBe` [
          Identifier "Data.Map" "fromList"
        , Identifier "Data.Map.Lazy" "fromList"
        , Identifier "Data.Map.Strict" "fromList"
        , Identifier "Data.Map.Strict.Internal" "fromList"
        , Identifier "Data.Map.Internal" "fromList"
        , Identifier "Data.List.NonEmpty" "fromList"
        , Identifier "Data.Set" "fromList"
        ]

  describe "extractIdentifiers" $ do
    it "extracts identifiers" $ do
      extractIdentifiers ".. `foldl' ..., `foldr' .." `shouldBe` [IdentifierInScope "foldl", IdentifierInScope "foldr"]

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
    -}
