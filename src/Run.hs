module Run where

import           Control.Exception
import           Data.Foldable

import           Interpreter (Interpreter)
import qualified Interpreter

run :: [String] -> String -> IO ()
run args input = bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
  trigger interpreter
  forM_ (lines input) $ \_ -> do
    trigger interpreter

trigger :: Interpreter -> IO ()
trigger interpreter = do
  Interpreter.reload interpreter >>= putStr
  Interpreter.hspec interpreter >>= putStr
