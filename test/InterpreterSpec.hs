{-# LANGUAGE RecordWildCards #-}
module InterpreterSpec (spec) where

import           Language.Haskell.GhciWrapper (eval)
import           System.Environment.Compat

import           Helper

import qualified Interpreter
import           Interpreter (Session(..), Summary(..), hspecFailureEnvName, hspecPreviousSummary)

spec :: Spec
spec = do
  describe "new" $ do
    it "unsets HSPEC_FAILURES" $ do
      setEnv hspecFailureEnvName "foo"
      withInterpreter [] $ \Session{..} -> do
        _ <- eval sessionInterpreter "import System.Environment"
        eval sessionInterpreter ("lookupEnv " ++ show hspecFailureEnvName) `shouldReturn` "Nothing\n"

  describe "reload" $ do
    it "reloads" $ do
      withInterpreter [] $ \session -> do
        silence (Interpreter.reload session) `shouldReturn` "Ok, modules loaded: none.\n"

  describe "hasSpec" $ around_ withSomeSpec $ do
    context "when module contains spec" $ do
      it "returns True" $ do
        withInterpreter ["Spec.hs"] $ \session -> do
          _ <- silence (Interpreter.reload session)
          Interpreter.hasSpec session `shouldReturn` True

    context "when module does not contain spec" $ do
      it "returns False" $ do
        withInterpreter ["Spec.hs"] $ \session -> do
          writeFile "Spec.hs" "module Main where"
          _ <- silence (Interpreter.reload session)
          Interpreter.hasSpec session `shouldReturn` False

  describe "runSpec" $ around_ withSomeSpec $ do
    it "stores summary of spec run" $ do
      withInterpreter ["Spec.hs"] $ \session -> do
        _ <- silence (Interpreter.runSpec session >> Interpreter.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 2 0)

    it "accepts Hspec args" $ do
      withInterpreter ["Spec.hs", "--no-color", "-m", "foo"] $ \session -> do
        _ <- silence (Interpreter.runSpec session >> Interpreter.runSpec session)
        hspecPreviousSummary session `shouldReturn` Just (Summary 1 0)
