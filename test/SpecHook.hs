module SpecHook (
  hook
, ghcInfo
, getCacheDirectory
) where

import Imports

import Test.Hspec
import System.Process
import System.Directory
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Temp
import GHC.Conc

import GHC.Info qualified as GHC

installPackageEnvironment :: FilePath -> FilePath -> IO ()
installPackageEnvironment ghc file = do
  unsetEnv "GHC_ENVIRONMENT"
  callProcess "cabal" $
    ["install", "-v0", "-w", ghc, "-z", "--lib", "--avoid-reinstalls", "--package-env", file] ++ packages
  where
    packages = ["hspec", "hspec-meta", "markdown-unlit", "binary", "--constraint", "binary installed"]

ensurePackageEnvironment :: FilePath -> FilePath -> IO ()
ensurePackageEnvironment ghc file = doesFileExist file >>= \ case
  False -> installPackageEnvironment ghc file
  True -> pass

setPackageEnvironment :: IO ()
setPackageEnvironment = do
  lookupEnv "SENSEI_TEST_GHC" >>= maybe pass (setEnv GHC.sensei_ghc)
  dir <- getCacheDirectory
  info <- ghcInfo
  let file = dir </> info.ghcVersionString <.> "ghc" <.> "env"
  ensurePackageEnvironment info.ghc file
  setEnv "GHC_ENVIRONMENT" file

hook :: Spec -> Spec
hook spec = runIO (setPackageEnvironment >> getNumProcessors >>= setNumCapabilities) >> parallel spec

{-# NOINLINE ghcInfo #-}
ghcInfo :: IO GHC.Info
ghcInfo = unsafePerformIO do
  info <- GHC.info
  return $ return info

getCacheDirectory :: IO String
getCacheDirectory = lookupEnv "SENSEI_TEST_CACHE" >>= \ case
  Nothing -> do
    tmp <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory tmp "sensei-tests"
    setEnv "SENSEI_TEST_CACHE" dir
    return dir
  Just dir -> do
    return dir
