module GHC.GhcPkg (dump) where

import Imports

import Distribution.InstalledPackageInfo
import System.Process (readProcess)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

dump :: IO [InstalledPackageInfo]
dump = ghcPkgDump >>= mapM (readPackageConfig . T.encodeUtf8)

ghcPkgDump :: IO [Text]
ghcPkgDump = readProcess "ghc-pkg" ["dump"] "" <&> map expandPkgRoot . split
  where
    split :: String -> [Text]
    split = T.splitOn "---\n" . pack

    expandPkgRoot :: Text -> Text
    expandPkgRoot input = case T.breakOnEnd "\n" (T.stripEnd input) of
      (conf, parsePkgRoot -> Just pkgroot) -> T.replace "${pkgroot}" pkgroot conf
      _ -> input

    parsePkgRoot :: Text -> Maybe Text
    parsePkgRoot = T.stripPrefix "pkgroot: " >=> unpack >>> readMaybe

readPackageConfig :: ByteString -> IO InstalledPackageInfo
readPackageConfig input = case parseInstalledPackageInfo input of
  Left err -> die . unlines $
      "Parsing the output of `ghc-pkg dump` failed!"
    : ""
    : NonEmpty.toList err
  Right (_, package) -> return package
