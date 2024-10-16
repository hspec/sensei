{-# LANGUAGE CPP #-}
module TriggerSpec (spec) where

import           Helper

import qualified Data.Text as Text

import qualified Session
import           Session (Session)
import           Language.Haskell.GhciWrapper (Config(..))

import           Trigger hiding (trigger, triggerAll)
import qualified Trigger

normalize :: String -> [String]
normalize = normalizeTiming . lines . forGhc9dot4
  where
    normalizeTiming :: [String] -> [String]
    normalizeTiming = normalizeLine "Finished in "

    normalizeLine :: String -> [String] -> [String]
    normalizeLine message = map f
      where
        f line
          | message `isPrefixOf` line = message ++ "..."
          | otherwise = line

    forGhc9dot4 :: String -> String
    forGhc9dot4 = Text.unpack . Text.replace "Ok, modules loaded: Spec." "Ok, modules loaded: Spec (Spec.o)." . Text.pack

withSession :: FilePath -> [String] -> (Session -> IO a) -> IO a
withSession specPath args = do
  Session.withSession ghciConfig {configWorkingDirectory = Just dir} $
      "-fhide-source-paths"
    : "-fno-diagnostics-show-caret"
    : "-fdiagnostics-color=never"
    : file
    : args
    ++ ["--no-color", "--seed=0"]
  where
    (dir, file) = splitFileName specPath

trigger :: Session -> IO (Result, [String])
trigger session = triggerWithHooks session defaultHooks

triggerWithHooks :: Session -> Hooks -> IO (Result, [String])
triggerWithHooks session hooks = fmap normalize <$> Trigger.trigger session hooks

triggerAll :: Session -> IO (Result, [String])
triggerAll session = fmap normalize <$> Trigger.triggerAll session defaultHooks

requiresHspecMeta :: IO () -> IO ()
requiresHspecMeta action = try action >>= \ case
  Left (ExitFailure 1) -> expectationFailure $ unlines [
      "This tests requires `hspec-meta`, which is not available.  To address this run"
    , ""
    , "    echo | cabal repl sensei --build-depends hspec-meta"
    , ""
    , "once."
    ]
  Left err -> throwIO err
  Right () -> pass

data HookExecuted = BeforeReloadSucceeded | AfterReloadSucceeded
  deriving (Eq, Show)

withHooks :: (Hooks -> IO ()) -> IO [HookExecuted]
withHooks action = withSpy $ \ spy -> action defaultHooks {
  beforeReload = spy BeforeReloadSucceeded >> return HookSuccess
, afterReload = spy AfterReloadSucceeded >> return HookSuccess
}

failingHook :: Hook
failingHook = return $ HookFailure "hook failed"

spec :: Spec
spec = do
  describe "removeProgress" $ do
    it "removes transient output" $ do
      (removeProgress . unlines) [
          "foo"
        , "some progress output...\r                       \rbar"
        , "baz"
        ] `shouldBe` unlines [
          "foo"
        , "bar"
        , "baz"
        ]

  describe "triggerAll" $ do
    it "runs all specs" $ do
      withSomeSpec $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          (trigger session >> triggerAll session) `shouldReturn` (Failure, [
              modulesLoaded Ok ["Spec"]
            , withColor Green "RELOADING SUCCEEDED"
            , ""
            , "foo [✔]"
            , "bar [✘]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:8:3: "
            , "  1) bar"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in ..."
            , "2 examples, 1 failure"
            , "Summary {summaryExamples = 2, summaryFailures = 1}"
            ])

  describe "trigger" $ around withSomeSpec $ do
    it "reloads and runs specs" $ \ name -> do
      withSession name [] $ \ session -> do
        trigger session `shouldReturn` (Success, [
            modulesLoaded Ok ["Spec"]
          , withColor Green "RELOADING SUCCEEDED"
          , ""
          , "foo [✔]"
          , "bar [✔]"
          , ""
          , "Finished in ..."
          , "2 examples, 0 failures"
          , "Summary {summaryExamples = 2, summaryFailures = 0}"
          ])

    context "with hooks" $ do
      it "executes hooks" $ \ name -> do
        withHooks $ \ hooks -> do
          withSession name [] $ \ session -> do
            triggerWithHooks session hooks `shouldReturn` (Success, [
                modulesLoaded Ok ["Spec"]
              , withColor Green "RELOADING SUCCEEDED"
              , ""
              , "foo [✔]"
              , "bar [✔]"
              , ""
              , "Finished in ..."
              , "2 examples, 0 failures"
              , "Summary {summaryExamples = 2, summaryFailures = 0}"
              ])
        `shouldReturn` [BeforeReloadSucceeded, AfterReloadSucceeded]

      context "when the before-reload hook fails" $ do
        it "cancels the reload cycle" $ \ name -> do
          withHooks $ \ hooks -> do
            withSession name [] $ \ session -> do
              triggerWithHooks session hooks { beforeReload = failingHook } `shouldReturn` (HookFailed, [
                  "hook failed"
                ])
          `shouldReturn` []

      context "when the after-reload hook fails" $ do
        it "cancels the reload cycle" $ \ name -> do
          withHooks $ \ hooks -> do
            withSession name [] $ \ session -> do
              triggerWithHooks session hooks { afterReload = failingHook } `shouldReturn` (HookFailed, [
                  modulesLoaded Ok ["Spec"]
                , withColor Green "RELOADING SUCCEEDED"
                , "hook failed"
                ])
          `shouldReturn` [BeforeReloadSucceeded]

    context "with a module that does not compile" $ do
      it "stops after reloading" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name (passingSpec ++ "foo = bar")
          (trigger session >> trigger session) `shouldReturn` (Failure, [
              "[1 of 1] Compiling Spec"
#if __GLASGOW_HASKELL__ < 910
            , ""
#endif
#if __GLASGOW_HASKELL__ >= 906
            , "Spec.hs:9:7: error: [GHC-88464] Variable not in scope: bar"
#else
            , "Spec.hs:9:7: error: Variable not in scope: bar"
#endif
#if __GLASGOW_HASKELL__ >= 910
            , ""
#endif
            , modulesLoaded Failed []
            , withColor Red "RELOADING FAILED"
            ])

    context "with a failing spec" $ do
      it "indicates failure" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          (Failure, xs) <- trigger session
          xs `shouldContain` [modulesLoaded Ok ["Spec"]]
          xs `shouldContain` ["2 examples, 1 failure"]

      it "only reruns failing specs" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          (trigger session >> trigger session) `shouldReturn` (Failure, [
              modulesLoaded Ok ["Spec"]
            , withColor Green "RELOADING SUCCEEDED"
            , ""
            , "bar [✘]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:8:3: "
            , "  1) bar"
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in ..."
            , "1 example, 1 failure"
            , "Summary {summaryExamples = 1, summaryFailures = 1}"
            ])

    context "after a failing spec passes" $ do
      it "runs all specs" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          _ <- trigger session
          writeFile name passingSpec
          trigger session `shouldReturn` (Success, [
              "[1 of 1] Compiling Spec [Source file changed]"
            , modulesLoaded Ok ["Spec"]
            , withColor Green "RELOADING SUCCEEDED"
            , ""
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "1 example, 0 failures"
            , "Summary {summaryExamples = 1, summaryFailures = 0}"
            , ""
            , "foo [✔]"
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            , "Summary {summaryExamples = 2, summaryFailures = 0}"
            ])

    context "with a module that does not expose a spec" $ do
      it "only reloads" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name "module Spec where"
          (trigger session >> trigger session) `shouldReturn` (Success, [
              modulesLoaded Ok ["Spec"]
            , withColor Green "RELOADING SUCCEEDED"
            ])

    context "with an hspec-meta spec" $ do
      it "reloads and runs spec" $ \ name -> do
        requiresHspecMeta $ withSession name ["-package hspec-meta"] $ \ session -> do
          writeFile name passingMetaSpec
          (trigger session >> trigger session) `shouldReturn` (Success, [
              modulesLoaded Ok ["Spec"]
            , withColor Green "RELOADING SUCCEEDED"
            , ""
            , "foo [✔]"
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            , "Summary {summaryExamples = 2, summaryFailures = 0}"
            ])
