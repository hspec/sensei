module SpecHook where

import           Helper
import           System.Environment
import           GHC.Conc

import qualified Language.Haskell.GhciWrapper as Interpreter

installPackageEnvironment :: FilePath -> FilePath -> IO ()
installPackageEnvironment ghc file = callProcess "cabal" ["install", "-v0", "-w", ghc, "-z", "--lib", "hspec", "hspec-meta", "--package-env", file]

ensurePackageEnvironment :: FilePath -> FilePath -> IO ()
ensurePackageEnvironment ghc file = doesFileExist file >>= \ case
  False -> installPackageEnvironment ghc file
  True -> pass

getGhcVersion :: FilePath -> IO String
getGhcVersion ghc = do
  ghcVersion <- Interpreter.numericVersion ghc
  setEnv Interpreter.sensei_ghc_version ghcVersion
  return ghcVersion

setPackageEnvironment :: IO ()
setPackageEnvironment = do
  lookupEnv "SENSEI_TEST_GHC" >>= maybe pass (setEnv Interpreter.sensei_ghc)
  dir <- getCurrentDirectory
  env <- getEnvironment
  let ghc = Interpreter.lookupGhc env
  ghcVersion <- getGhcVersion ghc
  let file = dir </> "dist-newstyle" </> "test-env" </> ghcVersion
  ensurePackageEnvironment ghc file
  setEnv "GHC_ENVIRONMENT" file

hook :: Spec -> Spec
hook spec = runIO (setPackageEnvironment >> getNumProcessors >>= setNumCapabilities) >> parallel spec
