module Run where

import           Control.Exception
import           Data.Foldable
import           Data.List

import           Interpreter (Interpreter)
import qualified Interpreter

run :: [String] -> String -> IO ()
run args input = bracket (Interpreter.new args) Interpreter.close $ \interpreter -> do
  trigger interpreter >> return ()
  forM_ (lines input) $ \_ -> do
    trigger interpreter

trigger :: Interpreter -> IO String
trigger interpreter = do
  xs <- Interpreter.reload interpreter
  ys <- if "Ok, modules loaded:" `isInfixOf` xs
    then Interpreter.hspec interpreter
    else return ""
  return (xs ++ ys)
