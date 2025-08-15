module Options.GHCSpec (spec) where

import Prelude (putStrLn)
import Helper

import Options.GHC qualified as GHC


import GHC.Driver.CmdLine
import GHC.Driver.Session (flagsAll)


flags :: [String]
flags = sort $ {- filter (not . isPrefixOf "f") $ -} map ("-" <>) $ mapMaybe f flagsAll
  where
    f :: Flag a -> Maybe String
    f flag = flagName <$> case flag.flagGhcMode of
      OnlyGhc -> Nothing
      OnlyGhci -> g flag
      AllModes -> g flag
      HiddenFlag -> Nothing
      -- HiddenFlag -> g flag

      -- OnlyGhci -> Nothing
      -- AllModes -> Nothing

    g :: Flag a -> Maybe (Flag a)
    g flag = case flag.flagOptKind of
      NoArg _ -> Just flag
      _ -> Nothing

options :: [String]
options = sort $ {- filter (not . isPrefixOf "f") $ -} map ("-" <>) $ mapMaybe f flagsAll
  where
    f :: Flag a -> Maybe String
    f flag = flagName <$> case flag.flagGhcMode of
      OnlyGhc -> Nothing
      OnlyGhci -> g flag
      AllModes -> g flag
      HiddenFlag -> Nothing
      -- HiddenFlag -> g flag
      -- _ -> Nothing

      -- OnlyGhci -> Nothing
      -- AllModes -> Nothing

    g :: Flag a -> Maybe (Flag a)
    g flag = case flag.flagOptKind of
      NoArg _ -> Nothing          -- -f
      PassFlag _ -> Nothing       -- -f
      HasArg _ -> Nothing         -- -farg or -f=arg or -f arg
      SepArg _ -> Nothing         -- -f arg

      Prefix _ -> Nothing         -- -farg or -f=arg
      IntSuffix _ -> Nothing      -- -fnum or -f=num
      Word64Suffix _ -> Nothing   -- -fnum or -f=num
      FloatSuffix _ -> Nothing    -- -fnum or -f=num

      OptPrefix _ -> Nothing      -- -f -farg or -f=arg
      OptIntSuffix _ -> Nothing   -- -f -fnum or -f=num
      AnySuffix _ -> Nothing      -- -f or -farg
      -- _ -> Just flag
      --

spec :: Spec
spec = sequential do
  fdescribe "takeGhc" do
    context "HasArg" do
      it "" do
        GHC.takeGhc ["-trust", "base", "foo", "bar"] `shouldBe` (["-trust", "base"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-trust=", "base", "foo", "bar"] `shouldBe` (["-trust=", "base"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-trust=base", "foo", "bar"] `shouldBe` (["-trust=base"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-trustbase", "foo", "bar"] `shouldBe` (["-trustbase"], ["foo", "bar"])

    context "SepArg" do
      it "" do
        GHC.takeGhc ["-dinline-check", "foo", "bar"] `shouldBe` (["-dinline-check", "foo"], ["bar"])

    context "Prefix" do
      it "" do
        GHC.takeGhc ["-L/some/path", "foo", "bar"] `shouldBe` (["-L/some/path"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-L=/some/path", "foo", "bar"] `shouldBe` (["-L=/some/path"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-L", "foo", "bar"] `shouldBe` ([], ["-L", "foo", "bar"])

      it "" do
        GHC.takeGhc ["-L=", "foo", "bar"] `shouldBe` ([], ["-L=", "foo", "bar"])

    context "OptPrefix" do
      it "" do
        GHC.takeGhc ["-isrc", "foo", "bar"] `shouldBe` (["-isrc"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-i=src", "foo", "bar"] `shouldBe` (["-i=src"], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-i=", "foo", "bar"] `shouldBe` (["-i="], ["foo", "bar"])

      it "" do
        GHC.takeGhc ["-i", "foo", "bar"] `shouldBe` (["-i"], ["foo", "bar"])


  describe "flags" $ do
    it "reverses a list" $ do
      sort GHC.flags `shouldBe` flags

    fit "reverses a list" $ do
      putStrLn "***********************************************************"
      -- traverse_ print $ mapMaybe (stripPrefix "-fno-") flags
      traverse_ print options
      putStrLn "***********************************************************"
      pending

    it "reverses a list" $ do
      sort GHC.xflags `shouldBe` (mapMaybe (stripPrefix "X") flags)

    it "reverses a list" $ do
      sort GHC.warningFlags `shouldBe` (filter (isPrefixOf "-W") flags)
