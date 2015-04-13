module InterpreterSpec (main, spec) where

import           Test.Hspec
import           Data.List
import           Control.Exception

import           Interpreter (Interpreter)
import qualified Interpreter

main :: IO ()
main = hspec spec

withInterpreter :: [String] -> (Interpreter -> IO a) -> IO a
withInterpreter args action = bracket (Interpreter.new $ "-ignore-dot-ghci" : args) Interpreter.close action

spec :: Spec
spec = do
  describe "reload" $ do
    it "reloads" $ do
      withInterpreter [] $ \interpreter -> do
        Interpreter.reload interpreter `shouldReturn` "Ok, modules loaded: none.\n"

  describe "hspec" $ do
    it "runs specs" $ do
      withInterpreter ["resource/Spec.hs"] $ \interpreter -> do
        xs <- (Interpreter.hspec interpreter >> Interpreter.hspec interpreter)
        xs `shouldSatisfy` ("1 example, 0 failures" `isInfixOf`)
