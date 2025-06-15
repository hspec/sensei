module GHC.EnvironmentFileSpec (spec) where

import Helper
import System.Process
import Distribution.Types.InstalledPackageInfo (libraryDirs)

import GHC.EnvironmentFile

spec :: Spec
spec = do
  describe "listHieFiles" do
    it "boot package" do
      info <- ghcInfo
      packageConfigFile <- strip <$> readProcess "find" [info.globalPackageDb, "-name", "binary-*.conf"] ""
      p <- readPackageConfig $ packageConfigFile

      for_ (libraryDirs p) \ dir -> doesDirectoryExist dir >>= flip unless do
        expectationFailure $ unwords [show dir, "does not exist!"]

      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> info.ghcVersionString)
      Just dir <- determineHieDirectory boot p
      listHieFiles dir p `shouldReturn` [
          dir </> "Data/Binary.hie"
        , dir </> "Data/Binary/Builder.hie"
        , dir </> "Data/Binary/Get.hie"
        , dir </> "Data/Binary/Get/Internal.hie"
        , dir </> "Data/Binary/Put.hie"
        ]

    it "regular package" do
      info <- ghcInfo
      let
        command :: CreateProcess
        command = shell $ "find ~/.local/state/cabal/store/ghc-" <> info.ghcVersionString <> "-* -name 'hspec-core-*.conf'"

      packageConfigFile : _ <- reverse . lines <$> readCreateProcess command ""


      p <- readPackageConfig packageConfigFile

      boot <- getXdgDirectory XdgState $ "ghc-hie-files" </> ("ghc-" <> info.ghcVersionString)
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

  describe "parseEntries" do
    it "accepts clear-package-db" do
      parseEntries "/path/to/env/file" "clear-package-db"
        `shouldBe` Right [ClearPackageDb]

    it "accepts global-package-db" do
      parseEntries "/path/to/env/file" "global-package-db"
        `shouldBe` Right [GlobalPackageDb]

    it "accepts user-package-db" do
      parseEntries "/path/to/env/file" "user-package-db"
        `shouldBe` Right [UserPackageDb]

    it "accepts absolute path to package-db" do
      parseEntries "/path/to/env/file" "package-db /path/to/package/db"
        `shouldBe` Right [PackageDb "/path/to/package/db"]

    it "accepts relative path to package-db" do
      parseEntries "/path/to/env/file" "package-db package/db"
        `shouldBe` Right [PackageDb "/path/to/env/package/db"]

    it "accepts package-id" do
      parseEntries "/path/to/env/file" "package-id base-4.20.0.0-18ae"
        `shouldBe` Right [PackageId "base-4.20.0.0-18ae"]

    it "accepts raw package-id" do
      parseEntries "/path/to/env/file" "base-4.20.0.0-18ae"
        `shouldBe` Right [PackageId "base-4.20.0.0-18ae"]

    it "ignores comments" do
      parseEntries "/path/to/env/file" "-- some comment\nbase-4.20.0.0-18ae"
        `shouldBe` Right [PackageId "base-4.20.0.0-18ae"]

    it "fails on invalid entry" do
      parseEntries "/path/to/env/file" "foo bar"
        `shouldBe` Left "Can't parse environment file entry: /path/to/env/file: foo bar"
