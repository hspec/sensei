module Options (splitArgs) where

import           Imports
import           Data.Coerce (coerce)
import qualified GHC.List as List

import           System.Console.GetOpt
import qualified Options.GHC as GHC

splitArgs :: [String] -> ([String], [String])
splitArgs args = case break (== "--") $ reverse args of
  (xs, "--" : ys) -> (reverse ys, reverse xs)
  _ -> partitionOptions $ classify args

newtype GhcOption = GhcOption [String]
newtype HspecOption = HspecOption [String]

type Option = Either GhcOption HspecOption

ghcOption :: [String] -> Option
ghcOption = Left . GhcOption

hspecOption :: [String] -> Option
hspecOption = Right . HspecOption

partitionOptions :: [Option] -> ([String], [String])
partitionOptions = bimap (List.concat . coerce) (List.concat . coerce) . partitionEithers

classify :: [String] -> [Option]
classify = GHC.takeGhc >>> \ case
  ([], args) -> case takeHspec args of
    ([], []) -> []
    ([], file : rest) -> ghcOption [file] : classify rest
    (hspec, rest) -> hspecOption hspec : classify rest
  (ghc, rest) -> ghcOption ghc : classify rest

takeHspec :: [String] -> ([String], [String])
takeHspec = \ case
  a : args | isHspecArgs [a] -> ([a], args)
  a : b : args | isHspecArgs [a, b] -> ([a, b], args)
  args -> ([], args)

isHspecArgs :: [String] -> Bool
isHspecArgs xs = case getOpt Permute options xs of
  (result, [], []) -> all (== Valid) result
  _ -> False

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
