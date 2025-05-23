{-# LANGUAGE CPP #-}
module TriggerSpec (spec) where

import           Helper

import qualified Session
import           Session (Session)
import           Language.Haskell.GhciWrapper (Config(..))

import           Trigger hiding (trigger, triggerAll)
import qualified Trigger

normalize :: String -> [String]
normalize = normalizeTiming . lines
  where
    normalizeTiming :: [String] -> [String]
    normalizeTiming = normalizeLine "Finished in "

    normalizeLine :: String -> [String] -> [String]
    normalizeLine message = map f
      where
        f line
          | message `isPrefixOf` line = message ++ "..."
          | otherwise = line

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

defaultHooks :: Hooks
defaultHooks = Hooks {
  beforeReload = return HookSuccess
, afterReload = return HookSuccess
}

trigger :: Session -> IO (Result, [String])
trigger session = triggerWithHooks session defaultHooks

triggerWithHooks :: Session -> Hooks -> IO (Result, [String])
triggerWithHooks session hooks = do
  modules <- newIORef []
  (result, output, _) <- Trigger.trigger modules session hooks
  return (result, normalize output)

triggerAll :: Session -> IO (Result, [String])
triggerAll session = do
  modules <- newIORef []
  (result, output, _) <- Trigger.triggerAll modules session defaultHooks
  return (result, normalize output)

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
              withColor Green "RELOADING SUCCEEDED"
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
            ])

  describe "trigger" $ around withSomeSpec $ do
    it "reloads and runs specs" $ \ name -> do
      withSession name [] $ \ session -> do
        trigger session `shouldReturn` (Success, [
            withColor Green "RELOADING SUCCEEDED"
          , ""
          , "foo [✔]"
          , "bar [✔]"
          , ""
          , "Finished in ..."
          , "2 examples, 0 failures"
          ])

    context "with hooks" $ do
      it "executes hooks" $ \ name -> do
        withHooks $ \ hooks -> do
          withSession name [] $ \ session -> do
            triggerWithHooks session hooks `shouldReturn` (Success, [
                withColor Green "RELOADING SUCCEEDED"
              , ""
              , "foo [✔]"
              , "bar [✔]"
              , ""
              , "Finished in ..."
              , "2 examples, 0 failures"
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
                  withColor Green "RELOADING SUCCEEDED"
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
            , withColor Red "RELOADING FAILED"
            ])

    context "with a failing spec" $ do
      it "indicates failure" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          (Failure, xs) <- trigger session
          xs `shouldBe` [
              "[1 of 1] Compiling Spec [Source file changed]"
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
            ]

      it "only reruns failing specs" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          (trigger session >> trigger session) `shouldReturn` (Failure, [
              withColor Green "RELOADING SUCCEEDED"
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
            ])

    context "after a failing spec passes" $ do
      it "runs all specs" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name failingSpec
          _ <- trigger session
          writeFile name passingSpec
          trigger session `shouldReturn` (Success, [
              "[1 of 1] Compiling Spec [Source file changed]"
            , withColor Green "RELOADING SUCCEEDED"
            , ""
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "1 example, 0 failures"
            , ""
            , "foo [✔]"
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            ])

    context "with a module that does not expose a spec" $ do
      it "only reloads" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name "module Spec where"
          (trigger session >> trigger session) `shouldReturn` (Success, [
              withColor Green "RELOADING SUCCEEDED"
            ])

    context "with an hspec-meta spec" $ do
      it "reloads and runs spec" $ \ name -> do
        withSession name [] $ \ session -> do
          writeFile name passingMetaSpec
          (trigger session >> trigger session) `shouldReturn` (Success, [
              withColor Green "RELOADING SUCCEEDED"
            , ""
            , "foo [✔]"
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            ])
