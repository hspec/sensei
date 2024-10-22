module SpecHook where

import           Helper
import           System.Environment
import           GHC.Conc

import           Language.Haskell.GhciWrapper (lookupGhc)

installPackageEnvironment :: FilePath -> FilePath -> IO ()
installPackageEnvironment ghc file = callProcess "cabal" ["install", "-v0", "-w", ghc, "-z", "--lib", "hspec", "hspec-meta", "--package-env", file]

ensurePackageEnvironment :: FilePath -> FilePath -> IO ()
ensurePackageEnvironment ghc file = doesFileExist file >>= \ case
  False -> installPackageEnvironment ghc file
  True -> pass

setPackageEnvironment :: IO ()
setPackageEnvironment = do
  dir <- getCurrentDirectory
  ghc <- lookupGhc <$> getEnvironment
  ghcVersion <- strip <$> readProcess ghc ["--numeric-version"] ""
  let file = dir </> "dist-newstyle" </> "test-env" </> ghcVersion
  ensurePackageEnvironment ghc file
  setEnv "GHC_ENVIRONMENT" file

hook :: Spec -> Spec
hook spec = runIO (setPackageEnvironment >> getNumProcessors >>= setNumCapabilities) >> parallel spec
