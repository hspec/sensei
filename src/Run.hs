module Run where

import           Control.Exception
import           Data.Foldable

import qualified Interpreter

run :: [String] -> String -> IO ()
run args input = bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
  forM_ (lines input) $ \_ -> do
    Interpreter.trigger interpreter >>= putStr
