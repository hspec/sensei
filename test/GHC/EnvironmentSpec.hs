{-# LANGUAGE QuasiQuotes #-}
module GHC.EnvironmentSpec (spec) where

import           Helper

import           GHC.Environment

spec :: Spec
spec = do
  describe "listHieFiles" do
    it "boot package" do
      p <- readPackageConfig "/home/sol/.ghcup/ghc/9.10.1/lib/ghc-9.10.1/lib/package.conf.d/binary-0.8.9.2-e89c.conf"
      let version = "9.10.1"
      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> version)
      Just dir <- determineHieDirectory boot p
      listHieFiles dir p `shouldReturn` [
          dir </> "Data/Binary.hie"
        , dir </> "Data/Binary/Builder.hie"
        , dir </> "Data/Binary/Get.hie"
        , dir </> "Data/Binary/Get/Internal.hie"
        , dir </> "Data/Binary/Put.hie"
        ]

    it "regular package" do
      p <- readPackageConfig "/home/sol/.local/state/cabal/store/ghc-9.10.1-7767/package.db/hspec-core-2.11.12-9227aa67a0416adeb5e46f63bb289da27190e607bb5babf8559bdf85eedeeeda.conf"
      let version = "9.10.1"
      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> version)
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
