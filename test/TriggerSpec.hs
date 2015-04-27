module TriggerSpec (spec) where

import           Helper
import           Data.List

import           Trigger

normalize :: String -> [String]
normalize = normalizeErrors . normalizeTiming . normalizeSeed . lines
  where
    normalizeErrors :: [String] -> [String]
    normalizeErrors = mkNormalize "Spec.hs:"

    normalizeTiming :: [String] -> [String]
    normalizeTiming = mkNormalize "Finished in "

    normalizeSeed :: [String] -> [String]
    normalizeSeed = mkNormalize "Randomized with seed "

    mkNormalize :: String -> [String] -> [String]
    mkNormalize message = map f
      where
        f line
          | message `isPrefixOf` line = message ++ "..."
          | otherwise = line

spec :: Spec
spec = do
  describe "trigger" $ around_ withSomeSpec $ do
    it "reloads and runs specs" $ do
      withInterpreter ["Spec.hs", "--no-color"] $ \ghci -> do
        result <- silence (trigger ghci >> trigger ghci)
        fmap normalize result `shouldBe` (True, [
            "Ok, modules loaded: Spec."
          , ""
          , "foo"
          , "bar"
          , ""
          , "Finished in ..."
          , "2 examples, 0 failures"
          , "Summary {summaryExamples = 2, summaryFailures = 0}"
          ])

    context "with a module that does not compile" $ do
      it "stops after reloading" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" (passingSpec ++ "foo = bar")
          (False, xs) <- silence (trigger ghci >> trigger ghci)
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , ""
            , "Spec.hs:..."
            , "Failed, modules loaded: none."
            ]

    context "with a failing spec" $ do
      it "indicates failure" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" failingSpec
          (False, xs) <- silence (trigger ghci)
          xs `shouldContain` "Ok, modules loaded:"
          xs `shouldContain` "2 examples, 1 failure"

      it "only reruns failing specs" $ do
        withInterpreter ["Spec.hs", "--no-color"] $ \ghci -> do
          writeFile "Spec.hs" failingSpec
          (False, xs) <- silence (trigger ghci >> trigger ghci)
          normalize xs `shouldBe` [
              "Ok, modules loaded: Spec."
            , ""
            , "bar FAILED [1]"
            , ""
            , "Failures:"
            , ""
            , "  1) bar"
            , "       expected: 42"
            , "        but got: 23"
            , ""
            , "Randomized with seed ..."
            , ""
            , "Finished in ..."
            , "1 example, 1 failure"
            , "Summary {summaryExamples = 1, summaryFailures = 1}"
            ]

    context "after a failing spec passes" $ do
      it "runs all specs" $ do
        withInterpreter ["Spec.hs", "--no-color"] $ \ghci -> do
          writeFile "Spec.hs" failingSpec
          _ <- silence (trigger ghci)
          writeFile "Spec.hs" passingSpec
          (True, xs) <- silence (trigger ghci)
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , "Ok, modules loaded: Spec."
            , ""
            , "bar"
            , ""
            , "Finished in ..."
            , "1 example, 0 failures"
            , "Summary {summaryExamples = 1, summaryFailures = 0}"
            , ""
            , "foo"
            , "bar"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            , "Summary {summaryExamples = 2, summaryFailures = 0}"
            ]

    context "with a module that does not expose a spec" $ do
      it "only reloads" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" "module Main where"
          silence (trigger ghci >> trigger ghci) `shouldReturn` (True, "Ok, modules loaded: Main.\n")
