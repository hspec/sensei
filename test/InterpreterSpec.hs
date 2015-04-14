module InterpreterSpec (main, spec) where

import           Helper

import qualified Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reload" $ do
    it "reloads" $ do
      withInterpreter [] $ \ghci -> do
        silence (Interpreter.reload ghci) `shouldReturn` "Ok, modules loaded: none.\n"

  describe "hspec" $ do
    it "runs specs" $ do
      withInterpreter ["resource/Spec.hs"] $ \ghci -> do
        xs <- silence (Interpreter.hspec ghci >> Interpreter.hspec ghci)
        xs `shouldContain` "1 example, 0 failures"
