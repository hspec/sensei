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
  describe "triggerAll" $ around_ withSomeSpec $ do
    it "runs all specs" $ do
      writeFile "Spec.hs" failingSpec
      withSession ["Spec.hs", "--no-color"] $ \session -> do
        (False, xs) <- silence (trigger session >> triggerAll session)
        normalize xs `shouldBe` [
            "Ok, modules loaded: Spec."
          , ""
          , "foo"
          , "bar FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  Spec.hs:9: "
          , "  1) bar"
          , ""
          , "Randomized with seed ..."
          , ""
          , "Finished in ..."
          , "2 examples, 1 failure"
          , "Summary {summaryExamples = 2, summaryFailures = 1}"
          ]

  describe "trigger" $ around_ withSomeSpec $ do
    it "reloads and runs specs" $ do
      withSession ["Spec.hs", "--no-color"] $ \session -> do
        result <- silence (trigger session >> trigger session)
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
        writeFile "Spec.hs" (passingSpec ++ "foo = bar")
        withSession ["Spec.hs"] $ \session -> do
          (False, xs) <- silence (trigger session >> trigger session)
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , ""
            , "Spec.hs:..."
            , "Failed, modules loaded: none."
            ]

    context "with a failing spec" $ do
      it "indicates failure" $ do
        writeFile "Spec.hs" failingSpec
        withSession ["Spec.hs"] $ \session -> do
          (False, xs) <- silence (trigger session)
          xs `shouldContain` "Ok, modules loaded:"
          xs `shouldContain` "2 examples, 1 failure"

      it "only reruns failing specs" $ do
        writeFile "Spec.hs" failingSpec
        withSession ["Spec.hs", "--no-color"] $ \session -> do
          (False, xs) <- silence (trigger session >> trigger session)
          normalize xs `shouldBe` [
              "Ok, modules loaded: Spec."
            , ""
            , "bar FAILED [1]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:9: "
            , "  1) bar"
            , ""
            , "Randomized with seed ..."
            , ""
            , "Finished in ..."
            , "1 example, 1 failure"
            , "Summary {summaryExamples = 1, summaryFailures = 1}"
            ]

    context "after a failing spec passes" $ do
      it "runs all specs" $ do
        pending
        withSession ["Spec.hs", "--no-color"] $ \session -> do
          writeFile "Spec.hs" failingSpec
          _ <- silence (trigger session)
          writeFile "Spec.hs" passingSpec
          (True, xs) <- silence (trigger session)
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
        writeFile "Spec.hs" "module Main where"
        withSession ["Spec.hs"] $ \session -> do
          silence (trigger session) `shouldReturn` (True, "Ok, modules loaded: Main.\n")
