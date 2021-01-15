module TriggerSpec (spec) where

import           Helper

import           Trigger

normalize :: String -> [String]
normalize = normalizeErrors . normalizeTiming . normalizeSeed . lines . stripAnsiColors
  where
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

    normalizeErrors :: [String] -> [String]
    normalizeErrors xs = case xs of
      y : ys | "Spec.hs:" `isPrefixOf` y -> "Spec.hs:..." : normalizeErrors (removeErrorDetails ys)
      y : ys -> y : normalizeErrors ys
      [] -> []

    removeErrorDetails :: [String] -> [String]
    removeErrorDetails xs = case xs of
      (_ : _ : ' ' : '|' : _) : ys -> removeErrorDetails ys
      _ -> xs

    stripAnsiColors xs = case xs of
      '\ESC' : '[' : ';' : ys | 'm' : zs <- dropWhile isNumber ys -> stripAnsiColors zs
      '\ESC' : '[' : ys | 'm' : zs <- dropWhile isNumber ys -> stripAnsiColors zs
      y : ys -> y : stripAnsiColors ys
      [] -> []

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

  describe "triggerAll" $ around_ withSomeSpec $ do
    it "runs all specs" $ do
      withSession ["Spec.hs", "--no-color"] $ \session -> do
        writeFile "Spec.hs" failingSpec
        (False, xs) <- silence (trigger session >> triggerAll session)
        normalize xs `shouldBe` [
            modulesLoaded Ok ["Spec"]
          , ""
          , "foo"
          , "bar FAILED [1]"
          , ""
          , "Failures:"
          , ""
          , "  Spec.hs:9:3: "
          , "  1) bar"
          , ""
          , "  To rerun use: --match \"/bar/\""
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
            modulesLoaded Ok ["Spec"]
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
        withSession ["Spec.hs"] $ \session -> do
          writeFile "Spec.hs" (passingSpec ++ "foo = bar")
          (False, xs) <- silence (trigger session >> trigger session)
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , ""
            , "Spec.hs:..."
            , modulesLoaded Failed []
            ]

    context "with a failing spec" $ do
      it "indicates failure" $ do
        withSession ["Spec.hs"] $ \session -> do
          writeFile "Spec.hs" failingSpec
          (False, xs) <- silence (trigger session)
          xs `shouldContain` modulesLoaded Ok ["Spec"]
          xs `shouldContain` "2 examples, 1 failure"

      it "only reruns failing specs" $ do
        withSession ["Spec.hs", "--no-color"] $ \session -> do
          writeFile "Spec.hs" failingSpec
          (False, xs) <- silence (trigger session >> trigger session)
          normalize xs `shouldBe` [
              modulesLoaded Ok ["Spec"]
            , ""
            , "bar FAILED [1]"
            , ""
            , "Failures:"
            , ""
            , "  Spec.hs:9:3: "
            , "  1) bar"
            , ""
            , "  To rerun use: --match \"/bar/\""
            , ""
            , "Randomized with seed ..."
            , ""
            , "Finished in ..."
            , "1 example, 1 failure"
            , "Summary {summaryExamples = 1, summaryFailures = 1}"
            ]

    context "after a failing spec passes" $ do
      it "runs all specs" $ do
        withSession ["Spec.hs", "--no-color"] $ \session -> do
          writeFile "Spec.hs" failingSpec
          _ <- silence (trigger session)
          writeFile "Spec.hs" passingSpec
          (True, xs) <- silence (trigger session)
          normalize xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , modulesLoaded Ok ["Spec"]
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
        withSession ["Spec.hs"] $ \session -> do
          writeFile "Spec.hs" "module Foo where"
          silence (trigger session >> trigger session) `shouldReturn` (True, modulesLoaded Ok ["Foo"] ++ "\n")

    context "with an hspec-meta spec" $ do
      it "reloads and runs spec" $ do
        withSession ["Spec.hs", "--no-color"] $ \session -> do
          writeFile "Spec.hs" passingMetaSpec
          result <- silence (trigger session >> trigger session)
          fmap normalize result `shouldBe` (True, [
              modulesLoaded Ok ["Spec"]
            , ""
            , "foo"
            , "bar"
            , ""
            , "Finished in ..."
            , "2 examples, 0 failures"
            , "Summary {summaryExamples = 2, summaryFailures = 0}"
            ])
