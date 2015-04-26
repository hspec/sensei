module TriggerSpec (spec) where

import           Helper
import           Data.List

import           Trigger

normalizeOutput :: String -> [String]
normalizeOutput = map normalize . lines
  where
    normalize line
      | message `isPrefixOf` line = message ++ "..."
      | otherwise = line
    message = "Spec.hs:11:7: Not in scope: "

spec :: Spec
spec = do
  describe "trigger" $ around_ withSomeSpec $ do
    it "reloads and runs specs" $ do
      withInterpreter ["Spec.hs"] $ \ghci -> do
        (True, xs) <- silence (trigger ghci >> trigger ghci)
        xs `shouldContain` "Ok, modules loaded:"
        xs `shouldContain` "1 example, 0 failures"

    context "with a module that does not compile" $ do
      it "stops after reloading" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" (someSpec ++ "foo = bar")
          (False, xs) <- silence (trigger ghci >> trigger ghci)
          normalizeOutput xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( Spec.hs, interpreted )"
            , ""
            , "Spec.hs:11:7: Not in scope: ..."
            , "Failed, modules loaded: none."
            ]

    context "with a failing spec" $ do
      it "indicates failure" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" failingSpec
          (False, xs) <- silence (trigger ghci >> trigger ghci)
          xs `shouldContain` "Ok, modules loaded:"
          xs `shouldContain` "1 example, 1 failure"

    context "with a module that does not expose a spec" $ do
      it "only reloads" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" "module Main where"
          silence (trigger ghci >> trigger ghci) `shouldReturn` (True, "Ok, modules loaded: Main.\n")
