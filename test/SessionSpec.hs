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

  describe "runSpec" $ around_ withSomeSpec $ do
    it "stores summary of spec run" $ do
      withSession ["Spec.hs"] $ \session -> do
        _ <- silence (Session.runSpec session >> Session.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 2 0)

    it "accepts Hspec args" $ do
      withSession ["Spec.hs", "--no-color", "-m", "foo"] $ \session -> do
        _ <- silence (Session.runSpec session >> Session.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 1 0)
