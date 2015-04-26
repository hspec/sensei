module InterpreterSpec (spec) where

import           Helper

import           Interpreter (Summary(..))
import qualified Interpreter

spec :: Spec
spec = do
  describe "reload" $ do
    it "reloads" $ do
      withInterpreter [] $ \ghci -> do
        silence (Interpreter.reload ghci) `shouldReturn` "Ok, modules loaded: none.\n"

  describe "hasSpec" $ around_ withSomeSpec $ do
    context "when module contains spec" $ do
      it "returns True" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          _ <- silence (Interpreter.reload ghci)
          Interpreter.hasSpec ghci `shouldReturn` True

    context "when module does not contain spec" $ do
      it "returns False" $ do
        withInterpreter ["Spec.hs"] $ \ghci -> do
          writeFile "Spec.hs" "module Main where"
          _ <- silence (Interpreter.reload ghci)
          Interpreter.hasSpec ghci `shouldReturn` False

  describe "runSpec" $ around_ withSomeSpec $ do
    it "runs specs" $ do
      withInterpreter ["Spec.hs"] $ \ghci -> do
        (_, summary) <- silence (Interpreter.runSpec ghci >> Interpreter.runSpec ghci)
        summary `shouldBe` Just (Summary 1 0)

    it "accepts Hspec args" $ do
      withInterpreter ["Spec.hs", "--no-color", "-m", "foo"] $ \ghci -> do
        (_, summary) <- silence (Interpreter.runSpec ghci >> Interpreter.runSpec ghci)
        summary `shouldBe` Just (Summary 0 0)
