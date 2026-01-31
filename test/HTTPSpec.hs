{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module HTTPSpec (spec) where

import           Helper hiding (appConfig, pending)
import qualified Helper
import qualified Prelude
import qualified VCR

import           Network.Wai (Application)
import           Test.Hspec.Wai
import qualified Data.ByteString as B
import           Data.Aeson (encode)

import           Config
import           Config.DeepSeek
import           Sensei.API hiding (get, post, config)
import           HTTP (AppConfig(..))
import qualified HTTP
import qualified Trigger
import qualified GHC.Diagnostic as Diagnostic

verbose :: Bool
verbose = False

putStrLn :: String -> IO ()
putStrLn
  | verbose = Prelude.putStrLn
  | otherwise = \ _ -> pass

appConfig :: FilePath -> HTTP.AppConfig
appConfig dir = (Helper.appConfig dir) { putStrLn }

spec :: Spec
spec = do
  describe "app" $ do
    let
      file :: FilePath
      file = "Foo.hs"

      withApp :: (Trigger.Result, String, [Annotated]) -> SpecWith (FilePath, Application) -> Spec
      withApp lastResult = around \ item -> withTempDirectory \ dir -> do
        item (dir, HTTP.app (appConfig dir) { getLastResult = return lastResult })

      withAppWithFailure :: FilePath -> SpecWith (FilePath, Application) -> Spec
      withAppWithFailure name = around \ item -> withTempDirectory \ dir -> do
        let
          testCaseDir :: FilePath
          testCaseDir = "ghc-diagnostics/fixtures" </> name

          copySource :: IO ()
          copySource = readFile (testCaseDir </> file) >>= writeFile (dir </> file)

          readErrFile :: IO (Maybe Annotated)
          readErrFile = fmap normalizeFileName <$> (B.readFile (testCaseDir </> "ghc-9.14" </> "err.yaml") >>= Diagnostic.parseAnnotated mempty)

        copySource
        Just err <- readErrFile
        config <- loadConfig

        let
          deepSeek :: Maybe DeepSeek
          deepSeek = config.deepSeek <|> Just (DeepSeek $ BearerToken "")

          app :: Application
          app = HTTP.app (appConfig dir) { deepSeek, getLastResult = return (Trigger.Failure, "", [err]) }

        item (dir, app)
        where
          normalizeFileName :: Annotated -> Annotated
          normalizeFileName annotated = annotated { diagnostic = normalizeDiagnostic annotated.diagnostic }

          normalizeDiagnostic :: Diagnostic -> Diagnostic
          normalizeDiagnostic err = err { span = normalizeSpan <$> err.span }

          normalizeSpan :: Span -> Span
          normalizeSpan span = span { file }

    describe "/" $ do
      context "on success" $ do
        withApp (Trigger.Success, withColor Green "success", []) $ do
          it "returns status 200" $ do
            get "/" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color" $ do
            it "keeps terminal sequences" $ do
              get "/?color" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color=true" $ do
            it "keeps terminal sequences" $ do
              get "/?color=true" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color=false" $ do
            it "removes terminal sequences" $ do
              get "/?color=false" `shouldRespondWith` "success"

          context "with an invalid value for ?color" $ do
            it "returns status 400" $ do
              get "/?color=some%20value" `shouldRespondWith` 400 { matchBody = "invalid value for color: some%20value" }

      context "on failure" $ do
        withApp (Trigger.Failure, withColor Red "failure", []) $ do
          it "return 500" $ do
            get "/" `shouldRespondWith` 500

    describe "/diagnostics" $ do
      let
        start :: Location
        start = Location 23 42

        span :: Maybe Span
        span = Just $ Span "Foo.hs" start start

        err :: Diagnostic
        err = diagnostic { span, message = ["failure"] }

        expected :: ResponseMatcher
        expected = fromString . decodeUtf8 $ to_json [err]

      withApp (Trigger.Failure, "", [Annotated err Nothing []]) $ do
        it "returns GHC diagnostics" $ do
          get "/diagnostics" `shouldRespondWith` expected

    describe "/quick-fix" $ do
      withAppWithFailure "use-TemplateHaskellQuotes" $ do
        it "applies quick fixes" $ do
          post "/quick-fix" "{}" `shouldRespondWith` "" { matchStatus = 204 }
          dir <- getState
          liftIO $ readFile (dir </> file) `shouldReturn` unlines [
              "{-# LANGUAGE TemplateHaskellQuotes #-}"
            , "module Foo where"
            , "foo = [|23|]"
            ]

      withAppWithFailure "not-in-scope-perhaps-use-one-of-these" $ do
        context "when \"choice\" is specified" $ do
          it "applies the selected solution" $ do
            post "/quick-fix" "{\"choice\":2}" `shouldRespondWith` "" { matchStatus = 204 }
            dir <- getState
            liftIO $ readFile (dir </> file) `shouldReturn` unlines [
                "module Foo where"
              , "foo = foldr"
              ]

    describe "/deep-fix" $ do
      aroundAll_ (VCR.with "test/fixtures/vcr-tape.yaml") . withAppWithFailure "use-TemplateHaskellQuotes" $ do
        it "applies quick fixes using DeepSeek" do
          post "/deep-fix" "{}" `shouldRespondWith` "" { matchStatus = 204 }
          dir <- getState
          liftIO $ readFile (dir </> file) `shouldReturn` unlines [
              "{-# LANGUAGE TemplateHaskell #-}"
            , "module Foo where"
            , "foo = [|23|]"
            ]

        it "takes programmer instructions into account" do
          let
            body :: LazyByteString
            body = encode $ DeepFixRequest (Just instructions)

            instructions :: Instructions
            instructions = Instructions span "fix this by setting the offending definition to undefined"

            span :: Span
            span = Span "Foo.hs" start start

            start :: Location
            start = Location 2 1

          post "/deep-fix" body `shouldRespondWith` "" { matchStatus = 204 }
          dir <- getState
          liftIO $ readFile (dir </> file) `shouldReturn` unlines [
              "module Foo where"
            , "foo = undefined"
            ]

    context "when querying a non-existing endpoint" $ withApp undefined $ do
      it "returns status 404" $ do
        get "/foo" `shouldRespondWith` 404 {matchBody = "404 Not Found"}

      context "with \"Accept: application/json\"" $ do
        it "returns a JSON error" $ do
          request "GET" "/foo" [("Accept", "application/json")] "" `shouldRespondWith` 404 {
            matchBody = fromString $ intercalate "\n" [
                "{"
              , "  \"title\": \"Not Found\","
              , "  \"status\": 404"
              , "}"
              ]
          }
