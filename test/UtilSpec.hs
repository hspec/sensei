{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (spec) where

import           Helper

import           System.Posix.Files
import           System.Process

import           Util

spec :: Spec
spec = do
  describe "isHaskell" $ do
    it "accepts hs files" $ do
      isHaskell "/foo/bar/.git/baz/foo.hs" `shouldBe` True

    it "accepts hs files" $ do
      isHaskell "/foo/bar/.git/baz/foo.lhs" `shouldBe` True

  describe "isBoring" $ do
    it "ignores files in .git/" $ do
      isBoring "/foo/bar/.git/baz/foo.txt" `shouldBe` True

    it "ignores files in dist/" $ do
      isBoring "/foo/bar/dist/baz/foo.txt" `shouldBe` True

  describe "normalizeTypeSignatures" $ do
    it "removes newlines from type signatures" $ do
      normalizeTypeSignatures "foo\n  :: Int" `shouldBe` "foo :: Int"

    it "replaces unicode characters" $ do
      normalizeTypeSignatures "head ∷ [a] → a" `shouldBe` "head :: [a] -> a"

  describe "dotGhciWritableByOthers" $ do
    around_ inTempDirectory $ do
      before_ (touch ".ghci" >> callCommand "chmod go-w .ghci") $ do
        it "returns False" $ do
          dotGhciWritableByOthers `shouldReturn` False

        context "when writable by group" $ do
          it "returns True" $ do
            callCommand "chmod g+w .ghci"
            dotGhciWritableByOthers `shouldReturn` True

        context "when writable by others" $ do
          it "returns True" $ do
            callCommand "chmod o+w .ghci"
            dotGhciWritableByOthers `shouldReturn` True

        context "when directory is writable by group" $ do
          it "returns True" $ do
            callCommand "chmod g+w ."
            dotGhciWritableByOthers `shouldReturn` True

      context "when .ghci does not exist" $ do
        it "returns False" $ do
          dotGhciWritableByOthers `shouldReturn` False

  describe "writableByOthers" $ do
    it "returns False for owner" $ do
      writableByOthers ownerWriteMode `shouldBe` False

    it "returns True for group" $ do
      writableByOthers groupWriteMode `shouldBe` True

    it "returns True for others" $ do
      writableByOthers otherWriteMode `shouldBe` True
