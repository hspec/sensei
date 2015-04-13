module InterpreterSpec (main, spec) where

import           Test.Hspec
import           Control.Exception

import qualified Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "trigger" $ do
    it "reloads" $ do
      bracket (Interpreter.new ["-ignore-dot-ghci"]) Interpreter.close $ \interpreter -> do
        Interpreter.trigger interpreter `shouldReturn` "Ok, modules loaded: none.\n"
