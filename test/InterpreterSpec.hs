module InterpreterSpec (spec) where

import           Helper

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
        xs <- silence (Interpreter.hspec ghci >> Interpreter.hspec ghci)
        xs `shouldContain` "1 example, 0 failures"

    it "accepts Hspec args" $ do
      withInterpreter ["Spec.hs", "-m", "foo"] $ \ghci -> do
        xs <- silence (Interpreter.hspec ghci >> Interpreter.hspec ghci)
        xs `shouldContain` "0 examples, 0 failures"
