{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module GHC.DiagnosticSpec (spec) where

import Helper hiding (diagnostic)
import Test.Hspec.Expectations.Contrib qualified as Hspec
import Text.RawString.QQ (r, rQ)

import System.Process
import System.Console.ANSI.Codes
import Control.Concurrent.Async qualified as Async
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString qualified as B
import Data.Map qualified as Map

import HIE qualified as HIE

import GHC.Diagnostic
import GHC.Diagnostic.Annotated

shouldAnnotate :: Bool
shouldAnnotate = True

addAnnotation :: String -> IO a -> IO a
addAnnotation m = if shouldAnnotate then Hspec.annotate m else id

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
  parseAnnotated getAvailableImports (encodeUtf8 json) >>= \ case
    Nothing -> do
      expectationFailure $ "Parsing JSON failed:\n\n" <> json
    Just annotated -> addAnnotation (separator <> err <> separator) do
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
      info <- ghcInfo
      let
        process :: CreateProcess
        process = proc info.ghc (["-XNoStarIsType", "-fno-code"] ++ args ++ extraArgs ++ [src])
      (_, _, err) <- readCreateProcessWithExitCode process ""
      return err

    translate :: String -> String
    translate = map \ case
      '‘' -> '`'
      '’' -> '\''
      c -> c

getAvailableImports :: IO AvailableImports
getAvailableImports = return $ Map.fromList [
    (Name VariableName "c2w", ["Data.ByteString.Internal"])
  , (Name VariableName "fromList", ["Data.Map"])
  , (Name VariableName "NoArg", [ProvidedBy "System.Console.GetOpt" (Just "ArgDescr")])
  ]

unindent :: String -> Text
unindent (T.pack >>> T.dropWhileEnd isSpace >>> T.lines -> input) = go input
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
notInScope = Just . NotInScope

foundHole :: Type -> [HoleFit] -> Maybe Annotation
foundHole type_ = Just . FoundHole type_

importName :: Module -> Text -> Solution
importName module_ = ImportName module_ Unqualified

spec :: Spec
spec = do
  describe "format" do
    test "not-in-scope" [] [r|
      module Foo where
      foo = c2w
      |] (notInScope "c2w") [importName "Data.ByteString.Internal" "c2w"]

    test "not-in-scope-qualified" [] [r|
      module Foo where
      foo = B.c2w
      |] (notInScope (RequiredVariable "B" "c2w" NoTypeSignature)) [ImportName "Data.ByteString.Internal" "B" "c2w"]

    test "not-in-scope-with-type" [] [r|
      module Foo where
      foo :: Int
      foo = bar "baz"
      |] (notInScope (RequiredVariable Unqualified "bar" "String -> Int")) []

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
      |] (notInScope "NoArg") [importName "System.Console.GetOpt" "ArgDescr(..)"]

    test "not-in-scope-data-with-type" [] [r|
      module Foo where

      foo :: Maybe Int
      foo = Foo (23 :: Int)
      |] (notInScope (RequiredVariable Unqualified "Foo" "Int -> Maybe Int")) [UseName "foo"]

    test "not-in-scope-pattern" [] [r|
      module Foo where
      foo = case undefined of
        NoArg -> undefined
      |] (notInScope "NoArg") [importName "System.Console.GetOpt" "ArgDescr(..)"]

    test "found-hole" [] [r|
      module Foo where
      foo :: FilePath -> IO String
      foo name = do
        r <- _ name
        return r
      |] (foundHole "FilePath -> IO String" [
        HoleFit "foo" "FilePath -> IO String"
      , HoleFit "readFile" "FilePath -> IO String"
      , HoleFit "readIO" "forall a. Read a => String -> IO a"
      , HoleFit "return" "forall (m :: Type -> Type) a. Monad m => a -> m a"
      , HoleFit "fail" "forall (m :: Type -> Type) a. MonadFail m => String -> m a"
      , HoleFit "pure" "forall (f :: Type -> Type) a. Applicative f => a -> f a"
      ]
      ) [
        UseName "foo"
      , UseName "readFile"
      , UseName "readIO"
      , UseName "return"
      , UseName "fail"
      , UseName "pure"
      ]

    test "found-hole-no-type" ["-fno-show-type-of-hole-fits"] [r|
      module Foo where
      foo :: FilePath -> IO String
      foo name = do
        r <- _ name
        return r
      |] (foundHole "FilePath -> IO String" [
        HoleFit "foo" NoTypeSignature
      , HoleFit "readFile" NoTypeSignature
      , HoleFit "readIO" NoTypeSignature
      , HoleFit "return" NoTypeSignature
      , HoleFit "fail" NoTypeSignature
      , HoleFit "pure" NoTypeSignature
      ]
      ) [
        UseName "foo"
      , UseName "readFile"
      , UseName "readIO"
      , UseName "return"
      , UseName "fail"
      , UseName "pure"
      ]

    test "found-hole-single-line" [] [r|
      {-# LINE 1 "A" #-}
      data A
      a :: A
      a = _
      |] (foundHole "A" [
        HoleFit "a" "A"
      ]
      ) [
        UseName "a"
      ]

    test "found-hole-named" [] [r|
      data A
      a :: A
      a = _foo
      |] (foundHole "A" [
        HoleFit "a" "A"
      ]
      ) [
        UseName "a"
      ]

    test "found-hole-multiline-signature" [] [r|
      a :: String -> String -> String -> String -> String -> String -> String -> String
      a = _
      |] (foundHole "String -> String -> String -> String -> String -> String -> String -> String" [
        HoleFit "a" "String -> String -> String -> String -> String -> String -> String -> String"
      , HoleFit "mempty" "forall a. Monoid a => a"
      ]
      ) [
        UseName "a"
      , UseName "mempty"
      ]

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
      qualifiedName "foo" `shouldBe` RequiredVariable Unqualified "foo" NoTypeSignature

    it "parses a qualified name" do
      qualifiedName "Foo.Bar.baz" `shouldBe` RequiredVariable "Foo.Bar" "baz" NoTypeSignature

    it "parses a name with type signature" do
      qualifiedName "Foo.Bar.baz :: Int -> Int" `shouldBe` RequiredVariable "Foo.Bar" "baz" "Int -> Int"

  describe "formatAnnotated" do
    it "formats an annotated diagnostic message" do
      let
        set = T.pack $ setSGRCode [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
        reset = T.pack $ setSGRCode [Reset]

      Just annotated <- B.readFile "test/fixtures/not-in-scope/err.json" >>= parseAnnotated getAvailableImports
      formatAnnotated 1 annotated `shouldBe` (2, T.unlines [
          "test/fixtures/not-in-scope/Foo.hs:2:7: error: [GHC-88464]"
        , "    Variable not in scope: c2w"
        , ""
        , set <> "    [1] " <> reset <> "import Data.ByteString.Internal (c2w)"
        , ""
        ])

  describe "analyzeAnnotation" do
    let
      withAvailableImports :: (AvailableImports -> IO a) -> IO a
      withAvailableImports item = do
        require GHC_908
        info <- ghcInfo
        HIE.with (\ _ -> pass) info \ _ getAvailable async -> do
          Async.wait async
          getAvailable >>= item

    aroundAll withAvailableImports do
      context "with NotInScope" do
        it "suggests functions" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = NotInScope (RequiredVariable Unqualified "unlit" NoTypeSignature)
          analyzeAnnotation availableImports annotation `shouldBe` [
              ImportName "Text.Markdown.Unlit" Unqualified "unlit"
            ]

        it "suggests constructors" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = NotInScope (RequiredVariable Unqualified "Option" NoTypeSignature)
          analyzeAnnotation availableImports annotation `shouldBe` [
              ImportName "System.Console.GetOpt" Unqualified "OptDescr(..)"
            ]

        it "does not suggest types" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = NotInScope (RequiredVariable Unqualified "OptDescr" NoTypeSignature)
          analyzeAnnotation availableImports annotation `shouldBe` [
            ]

      context "with TypeNotInScope" do
        it "suggests types" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = TypeNotInScope Unqualified "OptDescr"
          analyzeAnnotation availableImports annotation `shouldBe` [
              ImportName "System.Console.GetOpt" Unqualified "OptDescr"
            ]

        it "does not suggest constructors" \ availableImports -> do
          let
            annotation :: Annotation
            annotation = TypeNotInScope Unqualified "Option"
          analyzeAnnotation availableImports annotation `shouldBe` [
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
