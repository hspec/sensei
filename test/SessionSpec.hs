{-# LANGUAGE RecordWildCards #-}
module SessionSpec (spec) where

import           Helper

import           System.Environment.Blank (setEnv)

import           Language.Haskell.GhciWrapper (eval)
import qualified Session
import           Session (Config(..), Session(..), Summary(..), hspecPreviousSummary, hspecCommand)

withSession :: [String] -> (Session -> IO a) -> IO a
withSession = Session.withSession ghciConfig

spec :: Spec
spec = do
  describe "withSession" $ do
    it "unsets HSPEC_FAILURES" $ do
      setEnv "HSPEC_FAILURES" "foo" True
      withSession [] $ \ Session{..} -> do
        eval sessionInterpreter "System.Environment.lookupEnv \"HSPEC_FAILURES\"" `shouldReturn` "Nothing\n"

    context "with `:set +t +s`" $ do
      it "works just fine" $ do
        withTempDirectory $ \ dir -> do
          let
            config = ghciConfig {
              configIgnoreDotGhci = False
            , configWorkingDirectory = Just dir
            }
          writeFile (dir </> ".ghci") ":set +t +s"
          Session.withSession config [] $ \ Session{..} -> do
            eval sessionInterpreter "23" `shouldReturn` "23\n"

    context "with -XOverloadedStrings" $ do
      it "works just fine" $ do
        withSession ["-XOverloadedStrings", "-Wall", "-Werror"] $ \ Session{..} -> do
          eval sessionInterpreter "23 :: Int" `shouldReturn` "23\n"

  describe "reload" $ do
    it "reloads" $ do
      withSession [] $ \session -> do
        Session.reload session `shouldReturn` (modulesLoaded Ok [] ++ "\n")

  describe "hasSpec" $ around withSomeSpec $ do
    context "when module contains spec" $ do
      it "returns True" $ \ name -> do
        withSession [name] $ \session -> do
          _ <- Session.reload session
          Session.hasSpec hspecCommand session `shouldReturn` True

    context "when module does not contain spec" $ do
      it "returns False" $ \ name -> do
        withSession [name] $ \session -> do
          writeFile name "module Main where"
          _ <- Session.reload session
          Session.hasSpec hspecCommand session `shouldReturn` False

  describe "hasHspecCommandSignature" $ do
    let signature = "Test.Hspec.Runner.hspecResult spec :: IO Test.Hspec.Core.Runner.Summary"

    context "when input contains qualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature hspecCommand signature `shouldBe` True

      it "ignores additional output after summary" $ do
        (Session.hasHspecCommandSignature hspecCommand . unlines) [
            "bar"
          , signature
          , "foo"
          ] `shouldBe` True

    context "when input contains unqualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature hspecCommand "Test.Hspec.Runner.hspecResult spec :: IO Summary" `shouldBe` True

    context "when input dose not contain Hspec command signature" $ do
      it "returns False" $ do
        Session.hasHspecCommandSignature hspecCommand "foo" `shouldBe` False

  describe "runSpec" $ around withSomeSpec $ do
    let runSpec = Session.runSpec hspecCommand
    it "stores summary of spec run" $ \ name -> do
      withSession [name] $ \session -> do
        _ <- runSpec session >> runSpec session
        hspecPreviousSummary session `shouldReturn` Just (Summary 2 0)

    it "accepts Hspec args" $ \ name -> do
      withSession [name, "--no-color", "-m", "foo"] $ \session -> do
        _ <- runSpec session >> runSpec session
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
