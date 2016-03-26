module Options (
  parseArgs
, RunArgs (..)
, Mode (..)
) where

import           Data.List
import           System.Console.GetOpt

data Mode = Help | Run RunArgs deriving (Eq, Show)

data RunArgs = RunArgs {
  runGhciArgs :: [String]
, runHspecArgs :: [String]
} deriving (Eq, Show)

parseArgs :: [String] -> Mode
parseArgs ("--help":_) = Help
parseArgs args = splitArgs args

splitArgs :: [String] -> Mode
splitArgs args = case break (== "--") $ reverse args of
  (xs, "--" : ys) -> Run $ RunArgs (reverse ys) (reverse xs)
  _ -> case filter isHspecArgs $ tails args of
    x : _ -> Run $ RunArgs (dropEnd (length x) args) x
    [] -> Run $ RunArgs args []
  where
    isHspecArgs xs = case getOpt Permute options xs of
      (_, [], []) -> True
      _ -> False

    dropEnd :: Int -> [a] -> [a]
    dropEnd n = reverse . drop n . reverse

options :: [OptDescr ()]
options = map ($ "") [
    Option []  ["help"]             (NoArg ())
  , Option "m" ["match"]            (ReqArg (const ()) "PATTERN")
  , Option []  ["skip"]             (ReqArg (const ()) "PATTERN")
  , Option []  ["color"]            (NoArg ())
  , Option []  ["no-color"]         (NoArg ())
  , Option "f" ["format"]           (ReqArg (const ()) "FORMATTER")
  , Option "o" ["out"]              (ReqArg (const ()) "FILE")
  , Option []  ["depth"]            (ReqArg (const ()) "N")
  , Option "a" ["qc-max-success"]   (ReqArg (const ()) "N")
  , Option []  ["qc-max-size"]      (ReqArg (const ()) "N")
  , Option []  ["qc-max-discard"]   (ReqArg (const ()) "N")
  , Option []  ["seed"]             (ReqArg (const ()) "N")
  , Option []  ["print-cpu-time"]   (NoArg ())
  , Option []  ["dry-run"]          (NoArg ())
  , Option []  ["fail-fast"]        (NoArg ())
  , Option "r" ["rerun"]            (NoArg ())
  ]
