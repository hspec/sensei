module GHC.EnvironmentFileSpec (spec) where

import Helper
import Distribution.Types.InstalledPackageInfo (libraryDirs)

import GHC.EnvironmentFile

spec :: Spec
spec = do
  info <- runIO ghcInfo

  describe "listAllHieFiles" do
    it "inspects GHC_ENVIRONMENT and lists all HIE files for in-scope packages" do
      files <- snd <$> listAllHieFiles info

      let
        containing :: String -> [FilePath]
        containing xs = map (dropWhile (not . isUpper)) $ filter (isInfixOf xs) files

      -- regular package
      containing "/hspec-2" `shouldBe` [
          "Test/Hspec.hie"
        , "Test/Hspec/Discover.hie"
        , "Test/Hspec/Formatters.hie"
        , "Test/Hspec/QuickCheck.hie"
        , "Test/Hspec/Runner.hie"
        ]

      -- boot package
      containing "/binary" `shouldBe` [
          "Data/Binary.hie"
        , "Data/Binary/Builder.hie"
        , "Data/Binary/Get.hie"
        , "Data/Binary/Get/Internal.hie"
        , "Data/Binary/Put.hie"
        ]

  describe "readPackageConfig" do
    it "expands ${pkgroot}" do
      packageConfigFile <- strip <$> readProcess "find" [info.globalPackageDb, "-name", "binary-*.conf"] ""
      package <- readPackageConfig packageConfigFile
      for_ package.libraryDirs \ dir -> doesDirectoryExist dir >>= flip unless do
        expectationFailure $ unwords [show dir, "does not exist!"]

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
