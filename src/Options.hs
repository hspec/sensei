module Options (splitArgs) where

import           Imports

import           System.Console.GetOpt

splitArgs :: [String] -> ([String], [String])
splitArgs args = case break (== "--") $ reverse args of
  (xs, "--" : ys) -> (reverse ys, reverse xs)
  _ -> case filter isHspecArgs $ tails args of
    x : _ -> (dropEnd (length x) args, x)
    [] -> (args, [])
  where
    isHspecArgs :: [String] -> Bool
    isHspecArgs xs = case getOpt Permute options xs of
      (result, [], []) -> all (== Valid) result
      _ -> False

    dropEnd :: Int -> [a] -> [a]
    dropEnd n = reverse . drop n . reverse

data Valid = Valid | Invalid
  deriving (Eq, Show)

options :: [OptDescr Valid]
options = concat [
    flag "color"
  , flag "diff"
  , flag "dry-run"
  , flag "expert"
  , flag "fail-fast"
  , flag "fail-on=ITEMS"
  , flag "focused-only"
  , flag "pretty"
  , flag "randomize"
  , flag "strict"
  , flag "times"
  , flag "unicode"
  , noArg "" "help"
  , noArg "" "ignore-dot-hspec"
  , noArg "" "print-cpu-time"
  , noArg "" "rerun-all-on-success"
  , noArg "r" "rerun"
  , reqArg "" "depth" "N"
  , reqArg "" "diff-command" "CMD"
  , reqArg "" "diff-context" "N"
  , reqArg "" "failure-report" "FILE"
  , reqArg "" "jobs" "N"
  , reqArg "" "qc-max-discard" "N"
  , reqArg "" "qc-max-shrinks" "N"
  , reqArg "" "qc-max-size" "N"
  , reqArg "" "seed" "N"
  , reqArg "" "skip" "PATTERN"
  , reqArg "a" "qc-max-success" "N"
  , reqArg "f" "format" "NAME"
  , reqArg "m" "match" "PATTERN"
  , [Option "p" ["print-slow-items"] (OptArg (maybe Valid intArg) "N") ""]
  ]
  where
    flag :: String -> [OptDescr Valid]
    flag name = concat [
        noArg "" name
      , noArg "" ("no-" <> name)
      ]

    noArg :: [Char] -> String -> [OptDescr Valid]
    noArg short name = [Option short [name] (NoArg Valid) ""]

    reqArg :: [Char] -> String -> String -> [OptDescr Valid]
    reqArg short name arg = [Option short [name] (ReqArg (const Valid) arg) ""]

    intArg :: String -> Valid
    intArg = maybe Invalid (const Valid) . readMaybe @Int
