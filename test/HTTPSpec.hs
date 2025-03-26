{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module HTTPSpec (spec) where

import           Prelude hiding (putStrLn, span)
import qualified Prelude
import           Helper hiding (pending)
import qualified VCR

import           Network.Wai (Application)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (WaiSession(..))
import           Control.Monad.Trans.State (StateT(..))
import           Control.Monad.Trans.Reader (ReaderT(..))

import           Config
import           Config.DeepSeek
import qualified HTTP
import qualified Trigger

verbose :: Bool
verbose = False

putStrLn :: String -> IO ()
putStrLn
  | verbose = Prelude.putStrLn
  | otherwise = \ _ -> pass

withTape :: VCR.Tape -> WaiSession st a -> WaiSession st a
withTape tape = lift (VCR.with tape)
  where
    lift :: forall st a. (forall b. IO b -> IO b) -> WaiSession st a -> WaiSession st a
    lift f action = WaiSession $ ReaderT \ st -> ReaderT \ application -> StateT \ clientState -> do
      f $ runStateT (runReaderT (runReaderT (unWaiSession action) st) application) clientState

spec :: Spec
spec = do
  describe "app" $ do
    let
      withApp :: (Trigger.Result, String, [Diagnostic]) -> SpecWith (FilePath, Application) -> Spec
      withApp (return -> lastResult) = around \ item -> withTempDirectory \ dir -> do
        config <- loadConfig
        let
          deepSeek :: Maybe DeepSeek
          deepSeek = config.deepSeek <|> Just (DeepSeek $ BearerToken "")

          app :: Application
          app = HTTP.app putStrLn defaultConfig { deepSeek } dir lastResult
        item (dir, app)

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
        err = (diagnostic Error) { span, message = ["failure"] }

        expected :: ResponseMatcher
        expected = fromString . decodeUtf8 $ to_json [err]

      withApp (Trigger.Failure, "", [err]) $ do
        it "returns GHC diagnostics" $ do
          get "/diagnostics" `shouldRespondWith` expected

    describe "/quick-fix" $ do
      let
        file :: FilePath
        file = "Foo.hs"

        err :: Diagnostic
        err = (diagnostic Error) {
          span = Just $ Span file (Location 3 7) (Location 3 16)
        , code = Just 62330
        , message = ["Illegal underscores in integer literals"]
        , hints = ["Perhaps you intended to use NumericUnderscores"]
        }

        source :: String
        source = unlines [
            "module Foo where"
          , "foo :: Int"
          , "foo = 1_000_000"
          ]

        writeSource :: FilePath -> IO ()
        writeSource dir = writeFile (dir </> file) source

      withApp (Trigger.Failure, "", [err]) $ do
        beforeWith (\ st@(dir, _) -> writeSource dir >> return st) do
          it "applies quick fixes" $ do
            post "/quick-fix" "{}" `shouldRespondWith` "" { matchStatus = 204 }
            dir <- getState
            liftIO $ readFile (dir </> file) `shouldReturn` unlines [
                "{-# LANGUAGE NumericUnderscores #-}"
              , "module Foo where"
              , "foo :: Int"
              , "foo = 1_000_000"
              ]

          it "applies quick fixes using DeepSeek" >>> sequential $ withTape "test/vcr/tape.yaml" do
            post "/quick-fix" "{\"deep-seek\":true}" `shouldRespondWith` "" { matchStatus = 204 }
            dir <- getState
            liftIO $ readFile (dir </> file) `shouldReturn` unlines [
                "module Foo where"
              , "foo :: Int"
              , "foo = 1000000"
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
