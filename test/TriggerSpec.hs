{-# LANGUAGE CPP #-}
module TriggerSpec (spec) where

import           Helper

import qualified Session
import           Session (Session)
import           Language.Haskell.GhciWrapper (Config(..))

import           Trigger

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

spec :: Spec
spec = do
  describe "reloadedSuccessfully" $ do
    context "with GHC < 8.2.1" $ do
      it "detects success" $ do
        reloadedSuccessfully "Ok, modules loaded: Spec." `shouldBe` True

    context "with GHC >= 8.2.1" $ do
      context "with a single module" $ do
        it "detects success" $ do
          reloadedSuccessfully "Ok, 1 module loaded." `shouldBe` True

      context "with multiple modules" $ do
        it "detects success" $ do
          reloadedSuccessfully "Ok, 5 modules loaded." `shouldBe` True

    context "with GHC >= 8.2.2" $ do
      context "with a single module" $ do
        it "detects success" $ do
          reloadedSuccessfully "Ok, one module loaded." `shouldBe` True

      context "with multiple modules" $ do
        it "detects success" $ do
          reloadedSuccessfully "Ok, four modules loaded." `shouldBe` True

  describe "triggerAll" $ do
    it "runs all specs" $ do
      withSomeSpec $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name failingSpec
          (False, xs) <- trigger session >> triggerAll session
          normalize xs `shouldBe` [
              modulesLoaded Ok ["Spec"]
            , ""
            , "foo [✔]"
            , "bar [✘]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:8:3: "
            , "  1) bar"
            , ""
            , "  To rerun use: --match \"/bar/\""
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in ..."
            , "2 examples, 1 failure"
            , "Summary {summaryExamples = 2, summaryFailures = 1}"
            ]

  describe "trigger" $ around withSomeSpec $ do
    it "reloads and runs specs" $ \ name -> do
      withSession name [] $ \session -> do
        result <- trigger session >> trigger session
        fmap normalize result `shouldBe` (True, [
            modulesLoaded Ok ["Spec"]
          , ""
          , "foo [✔]"
          , "bar [✔]"
          , ""
          , "Finished in ..."
          , "2 examples, 0 failures"
          , "Summary {summaryExamples = 2, summaryFailures = 0}"
          ])

    context "with a module that does not compile" $ do
      it "stops after reloading" $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name (passingSpec ++ "foo = bar")
          (False, xs) <- trigger session >> trigger session
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec"
            , ""
            , "Spec.hs:9:7: error: Variable not in scope: bar"
            , modulesLoaded Failed []
            ]

    context "with a failing spec" $ do
      it "indicates failure" $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name failingSpec
          (False, xs) <- trigger session
          xs `shouldContain` modulesLoaded Ok ["Spec"]
          xs `shouldContain` "2 examples, 1 failure"

      it "only reruns failing specs" $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name failingSpec
          (False, xs) <- trigger session >> trigger session
          normalize xs `shouldBe` [
              modulesLoaded Ok ["Spec"]
            , ""
            , "bar [✘]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:8:3: "
            , "  1) bar"
            , ""
            , "  To rerun use: --match \"/bar/\""
            , ""
            , "Randomized with seed 0"
            , ""
            , "Finished in ..."
            , "1 example, 1 failure"
            , "Summary {summaryExamples = 1, summaryFailures = 1}"
            ]

    context "after a failing spec passes" $ do
      it "runs all specs" $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name failingSpec
          _ <- trigger session
          writeFile name passingSpec
          (True, xs) <- trigger session
          normalize xs `shouldBe` [
#if __GLASGOW_HASKELL__ < 904
              "[1 of 1] Compiling Spec"
#else
              "[1 of 1] Compiling Spec [Source file changed]"
#endif
            , modulesLoaded Ok ["Spec"]
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
            ]

    context "with a module that does not expose a spec" $ do
      it "only reloads" $ \ name -> do
        withSession name [] $ \session -> do
          writeFile name "module Foo where"
          (trigger session >> trigger session) `shouldReturn` (True, modulesLoaded Ok ["Foo"] ++ "\n")

    context "with an hspec-meta spec" $ do
      it "reloads and runs spec" $ \ name -> do
        withSession name ["-package hspec-meta"] $ \session -> do
          writeFile name passingMetaSpec
          result <- trigger session >> trigger session
          fmap normalize result `shouldBe` (True, [
              modulesLoaded Ok ["Spec"]
            , ""
            , "foo [✔]"
            , "bar [✔]"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            , "Summary {summaryExamples = 2, summaryFailures = 0}"
            ])
