
module Options (splitArgs
               ,SenseiArgs(..)
               ,withArgs
               ) where

import           Data.List
import           Options.Applicative
import           System.Console.GetOpt

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
  , Option []  ["dry-run"]          (NoArg ())
  , Option []  ["fail-fast"]        (NoArg ())
  , Option "r" ["rerun"]            (NoArg ())
  ]

data SenseiArgs = SenseiArgs { watch     :: String    -- ^ Pattern to filter files to be watched
                             , testFlag  :: Bool
                             , otherArgs :: [String]  -- ^ ghci and hspec arguments
                             } deriving (Show)

senseiArgsParser :: Parser SenseiArgs
senseiArgsParser = SenseiArgs
                   <$> watching
                   <*> testflag
                   <*> many (strArgument (metavar "-- ARGS (ghci or hspec args)"))
  where watching = strOption
                   ( long "watch"
                     <> short 'w'
                     <> metavar "PATTERN"
                     <> help "regex to what files to include in watching the directory"
                   ) <|> pure "^*$"

        testflag = flag True False
                   (long "only-compile"
                    <> short 'c'
                    <> help "Only compile, no tests run!"
                   )



withArgs :: (SenseiArgs -> IO ()) -> IO ()
withArgs run = execParser opts' >>= run
  where opts' = info (helper <*> senseiArgsParser)
                ( fullDesc
                  <> progDesc "Automatically compile and run Hspec tests on file modifications"
                )
