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

  describe "hspec" $ around_ withSomeSpec $ do
    it "runs specs" $ do
      withInterpreter ["Spec.hs"] $ \ghci -> do
        (_, summary) <- silence (Interpreter.hspec ghci >> Interpreter.hspec ghci)
        summary `shouldBe` Just (Summary 1 0)

    it "accepts Hspec args" $ do
      withInterpreter ["Spec.hs", "--no-color", "-m", "foo"] $ \ghci -> do
        (_, summary) <- silence (Interpreter.hspec ghci >> Interpreter.hspec ghci)
        summary `shouldBe` Just (Summary 0 0)
