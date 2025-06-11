{-# LANGUAGE QuasiQuotes #-}
module GHC.EnvironmentFileSpec (spec) where

import System.Process
import           Helper

import           GHC.EnvironmentFile

spec :: Spec
spec = do
  describe "listHieFiles" do
    it "boot package" do
      ghcVersion <- strip <$> readProcess "ghc" ["--numeric-version"] ""
      Just packageConfigDatabase <- getGlobalPackageDb
      packageConfigFile <- strip <$> readProcess "find" [packageConfigDatabase, "-name", "binary-*.conf"] ""
      p <- readPackageConfig $ packageConfigFile

      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> ghcVersion)
      Just dir <- determineHieDirectory boot p
      listHieFiles dir p `shouldReturn` [
          dir </> "Data/Binary.hie"
        , dir </> "Data/Binary/Builder.hie"
        , dir </> "Data/Binary/Get.hie"
        , dir </> "Data/Binary/Get/Internal.hie"
        , dir </> "Data/Binary/Put.hie"
        ]

    it "regular package" do
      ghcVersion <- strip <$> readProcess "ghc" ["--numeric-version"] ""

      xx : _ <- reverse . lines <$> readCreateProcess (shell $ "find ~/.local/state/cabal/store/ghc-" <> ghcVersion <> "-* -name 'hspec-core-*.conf'") ""


      p <- readPackageConfig xx

      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> ghcVersion)
      Just dir <- determineHieDirectory boot p
      listHieFiles dir p `shouldReturn` [
          dir </> "Test/Hspec/Core/Extension.hie"
        , dir </> "Test/Hspec/Core/Extension/Config.hie"
        , dir </> "Test/Hspec/Core/Extension/Item.hie"
        , dir </> "Test/Hspec/Core/Extension/Option.hie"
        , dir </> "Test/Hspec/Core/Extension/Spec.hie"
        , dir </> "Test/Hspec/Core/Extension/Tree.hie"
        , dir </> "Test/Hspec/Core/Format.hie"
        , dir </> "Test/Hspec/Core/Formatters.hie"
        , dir </> "Test/Hspec/Core/Formatters/V1.hie"
        , dir </> "Test/Hspec/Core/Formatters/V2.hie"
        , dir </> "Test/Hspec/Core/Hooks.hie"
        , dir </> "Test/Hspec/Core/QuickCheck.hie"
        , dir </> "Test/Hspec/Core/Runner.hie"
        , dir </> "Test/Hspec/Core/Spec.hie"
        , dir </> "Test/Hspec/Core/Util.hie"
        ]

  describe "parseEntry" do
    it "accepts clear-package-db" do
      parseEntry "/path/to/env/file" "clear-package-db"
        `shouldBe` Just ClearPackageDb

    it "accepts global-package-db" do
      parseEntry "/path/to/env/file" "global-package-db"
        `shouldBe` Just GlobalPackageDb

    it "accepts user-package-db" do
      parseEntry "/path/to/env/file" "user-package-db"
        `shouldBe` Just UserPackageDb

    it "accepts absolute path to package-db" do
      parseEntry "/path/to/env/file" "package-db /path/to/package/db"
        `shouldBe` Just (PackageDb "/path/to/package/db")

    it "accepts relative path to package-db" do
      parseEntry "/path/to/env/file" "package-db package/db"
        `shouldBe` Just (PackageDb "/path/to/env/package/db")

    it "accepts package-id" do
      parseEntry "/path/to/env/file" "package-id base-4.20.0.0-18ae"
        `shouldBe` Just (PackageId "base-4.20.0.0-18ae")

    it "accepts raw package-id" do
      parseEntry "/path/to/env/file" "base-4.20.0.0-18ae"
        `shouldBe` Just (PackageId "base-4.20.0.0-18ae")
