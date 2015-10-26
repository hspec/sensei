{-# LANGUAGE RecordWildCards #-}
module SessionSpec (spec) where

import           Language.Haskell.GhciWrapper (eval)
import           System.Environment.Compat

import           Helper

import qualified Session
import           Session (Session(..), Summary(..), hspecFailureEnvName, hspecPreviousSummary)

spec :: Spec
spec = do
  describe "new" $ do
    it "unsets HSPEC_FAILURES" $ do
      setEnv hspecFailureEnvName "foo"
      withSession [] $ \Session{..} -> do
        _ <- eval sessionInterpreter "import System.Environment"
        eval sessionInterpreter ("lookupEnv " ++ show hspecFailureEnvName) `shouldReturn` "Nothing\n"

  describe "reload" $ do
    it "reloads" $ do
      withSession [] $ \session -> do
        silence (Session.reload session) `shouldReturn` "Ok, modules loaded: none.\n"

  describe "hasSpec" $ around_ withSomeSpec $ do
    context "when module contains spec" $ do
      it "returns True" $ do
        withSession ["Spec.hs"] $ \session -> do
          _ <- silence (Session.reload session)
          Session.hasSpec session `shouldReturn` True

    context "when module does not contain spec" $ do
      it "returns False" $ do
        withSession ["Spec.hs"] $ \session -> do
          writeFile "Spec.hs" "module Main where"
          _ <- silence (Session.reload session)
          Session.hasSpec session `shouldReturn` False

  describe "hasHspecCommandSignature" $ do
    let signature = "Test.Hspec.Runner.hspecResult spec :: IO Test.Hspec.Core.Runner.Summary"

    context "when input contains qualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature signature `shouldBe` True

      it "ignores additional output after summary" $ do
        (Session.hasHspecCommandSignature . unlines) [
            "bar"
          , signature
          , "foo"
          ] `shouldBe` True

    context "when input contains unqualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature "Test.Hspec.Runner.hspecResult spec :: IO Summary" `shouldBe` True

    context "when input dose not contain Hspec command signature" $ do
      it "returns False" $ do
        Session.hasHspecCommandSignature "foo" `shouldBe` False

  describe "runSpec" $ around_ withSomeSpec $ do
    it "stores summary of spec run" $ do
      withSession ["Spec.hs"] $ \session -> do
        _ <- silence (Session.runSpec session >> Session.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 2 0)

    it "accepts Hspec args" $ do
      withSession ["Spec.hs", "--no-color", "-m", "foo"] $ \session -> do
        _ <- silence (Session.runSpec session >> Session.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 1 0)

  describe "parseSummary" $ do
    let summary = Summary 2 0

    it "parses summary" $ do
      Session.parseSummary (show summary) `shouldBe` Just summary

    it "ignores additional output before / after summary" $ do
      (Session.parseSummary . unlines) [
          "foo"
        , show summary
        , "bar"
        ] `shouldBe` Just summary

    it "gives last occurrence precedence" $ do
      (Session.parseSummary . unlines) [
          show (Summary 3 0)
        , show summary
        ] `shouldBe` Just summary

    it "ignores additional output at the beginning of a line (to cope with ansi escape sequences)" $ do
      Session.parseSummary ("foo " ++ show summary) `shouldBe` Just (Summary 2 0)
