{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (spec) where

import           Helper

import           System.Posix.Files
import           System.Process

import           Util

spec :: Spec
spec = do
  describe "isHaskell" $ do
    it "accepts hs files" $
      isHaskell "/foo/bar/.git/baz/foo.hs" `shouldBe` True

    it "accepts hs files" $
      isHaskell "/foo/bar/.git/baz/foo.lhs" `shouldBe` True

  describe "isBoring" $ do
    it "ignores files in .git/" $
      isBoring "/foo/bar/.git/baz/foo.txt" `shouldBe` True

    it "ignores files in dist/" $
      isBoring "/foo/bar/dist/baz/foo.txt" `shouldBe` True

  describe "normalizeTypeSignatures" $ do
    it "removes newlines from type signatures" $
      normalizeTypeSignatures "foo\n  :: Int" `shouldBe` "foo :: Int"

    it "replaces unicode characters" $
      normalizeTypeSignatures "head ∷ [a] → a" `shouldBe` "head :: [a] -> a"

  describe "dotGhciWritableByOthers" $
    around_ inTempDirectory $ do
      before_ (touch ".ghci" >> callCommand "chmod go-w .ghci") $ do
        it "returns False" $
          dotGhciWritableByOthers `shouldReturn` False

        context "when writable by group" $
          it "returns True" $ do
            callCommand "chmod g+w .ghci"
            dotGhciWritableByOthers `shouldReturn` True

        context "when writable by others" $
          it "returns True" $ do
            callCommand "chmod o+w .ghci"
            dotGhciWritableByOthers `shouldReturn` True

        context "when directory is writable by group" $
          it "returns True" $ do
            callCommand "chmod g+w ."
            dotGhciWritableByOthers `shouldReturn` True

      context "when .ghci does not exist" $
        it "returns False" $
          dotGhciWritableByOthers `shouldReturn` False

  describe "writableByOthers" $ do
    it "returns False for owner" $
      writableByOthers ownerWriteMode `shouldBe` False

    it "returns True for group" $
      writableByOthers groupWriteMode `shouldBe` True

    it "returns True for others" $
      writableByOthers otherWriteMode `shouldBe` True
