module Options (
  WatchMode(..)
, getWatchMode
, splitArgs
) where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Control.Exception
import           System.Environment.Blank
import           System.Console.GetOpt

data WatchMode = WatchHaskell | WatchAll
  deriving (Eq, Show)

getWatchMode :: IO WatchMode
getWatchMode = do
  mode <- fromMaybe "haskell" <$> getEnv "SENSEI_WATCH_MODE"
  case map toLower mode of
    "" -> return WatchHaskell
    "haskell" -> return WatchHaskell
    "all" -> return WatchAll
    unknown -> throwIO . ErrorCall $ "SENSEI_WATCH_MODE: unknown value " <> show unknown

splitArgs :: [String] -> ([String], [String])
splitArgs args = case break (== "--") $ reverse args of
  (xs, "--" : ys) -> (reverse ys, reverse xs)
  _ -> case filter isHspecArgs $ tails args of
    x : _ -> (dropEnd (length x) args, x)
    [] -> (args, [])
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
  , Option []  ["focused-only"]     (NoArg ())
  , Option []  ["dry-run"]          (NoArg ())
  , Option []  ["fail-fast"]        (NoArg ())
  , Option "r" ["rerun"]            (NoArg ())
  ]
