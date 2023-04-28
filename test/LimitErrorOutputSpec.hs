{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LimitErrorOutputSpec (spec) where

import           Helper
import           Data.IORef
import           Data.Text.Encoding (encodeUtf8)
import           System.Random

import           LimitErrorOutput

deriving instance Random Lines

withRandomChunkSizes :: [ByteString] -> ([ByteString] -> Expectation) -> Property
withRandomChunkSizes chunks = forAll (randomChunkSizes $ mconcat chunks)

spec :: Spec
spec = do
  describe "limitToTerminalSize" $ do
    it "limits output to the height of the terminal" $ do
      let
        input = [
            "Foo.hs:1:1: error\n"
          , "Foo.hs:2:1: error\n"
          , "Foo.hs:3:1: error\n"
          , "...\n"
          , "...\n"
          , "Foo.hs:4:1: error\n"
          , "Foo.hs:5:1: error\n"
          , "Foo.hs:6:1: error\n"
          ]
      forAll (choose (1, 10)) $ \ terminalHeight -> do
        withRandomChunkSizes input $ \ chunks -> do
          output <- newIORef mempty
          echo <- limitToTerminalSize terminalHeight maxBound $ \ chunk -> modifyIORef output (<> chunk)
          mapM_ echo chunks
          readIORef output `shouldReturn` mconcat (genericTake terminalHeight input)

    it "does not limit any output that occurs before the first error" $ do
      let
        input = [
            "...\n"
          , "...\n"
          , "Foo.hs:1:1: error\n"
          , "Foo.hs:2:1: error\n"
          , "Foo.hs:3:1: error\n"
          , "...\n"
          , "...\n"
          , "Foo.hs:4:1: error\n"
          , "Foo.hs:5:1: error\n"
          , "Foo.hs:6:1: error\n"
          ]
      forAll (choose (1, 10)) $ \ terminalHeight -> do
        withRandomChunkSizes input $ \ chunks -> do
          output <- newIORef mempty
          echo <- limitToTerminalSize terminalHeight maxBound $ \ chunk -> modifyIORef output (<> chunk)
          mapM_ echo chunks
          readIORef output `shouldReturn` mconcat (genericTake (terminalHeight + 2) input)

    context "with lines that are wider than the terminal" $ do
      it "takes terminal width into account" $ do
        let
          terminalWidth = 5
          input = [
              (0, "Foo.h")
            , (0, "s:1:1")
            , (0, ": err")
            , (0, "or\n")
            , (4, "12345\n")
            , (5, "\n")
            , (6, "678\n")
            , (9, "01234")
            , (9, "56789")
            , (9, "67890\n")
            ]

          expectedFor :: Lines -> ByteString
          expectedFor terminalHeight = mconcat . map snd $ takeWhile ((< terminalHeight) . fst)  input

        forAll (choose (1, 12)) $ \ terminalHeight -> do
          withRandomChunkSizes (map snd input) $ \ chunks -> do
            output <- newIORef mempty
            echo <- limitToTerminalSize terminalHeight terminalWidth $ \ chunk -> modifyIORef output (<> chunk)
            mapM_ echo chunks
            readIORef output `shouldReturn` expectedFor terminalHeight

  describe "requiredScreenEstate" $ do
    it "determines required number of terminal lines" $ do
      let
        terminalWidth = 5
        matrix = [
            ("\n", 1)
          , ("1\n", 1)
          , ("12\n", 1)
          , ("123\n", 1)
          , ("1234\n", 1)
          , ("12345\n", 1)
          , ("123456\n", 2)
          , ("1234567\n", 2)
          , ("12345678\n", 2)
          , ("123456789\n", 2)
          , ("1234567890\n", 2)
          , ("12345678901\n", 3)
          , ("123456789012\n", 3)
          , ("1234567890123\n", 3)
          , ("12345678901234\n", 3)
          , ("123456789012345\n", 3)
          , ("1234567890123456\n", 4)
          ]
      forM_ matrix $ \ (input, expected) -> do
        requiredScreenEstate terminalWidth input `shouldReturn` expected

    it "ignores ANSI escape sequences" $ do
      requiredScreenEstate 3 ("\ESC[31m" <> "foobar" <> "\ESC[0m") `shouldReturn` 2

    context "with Unicode text" $ do
      it "determines required number of terminal lines" $ do
        requiredScreenEstate 3 (encodeUtf8 "แอปเปิ้ลเขียว") `shouldReturn` 4

    context "with emojis" $ do
      it "determines required number of terminal lines" $ do
        requiredScreenEstate 5 (encodeUtf8 "🍏🍏🍏") `shouldReturn` 2

      it "correctly handles lines that can not be fully utilized" $ do
        requiredScreenEstate 3 (encodeUtf8 "🍏🍏🍏") `shouldReturn` 3

    context "with nonprintable characters" $ do
      it "returns 1" $ do
        requiredScreenEstate 3 "01234\ESC56789\n" `shouldReturn` 1

  describe "wcwidth" $ do
    context "with ASCII characters" $ do
      it "returns display width" $ do
        wcwidth 'a' `shouldReturn` 1

    context "with Unicode characters" $ do
      it "returns display width" $ do
        sum @[] <$> (mapM wcwidth "แอปเปิ้ลเขียว") `shouldReturn` 10

    context "with emojis" $ do
      it "returns display width" $ do
        wcwidth '🍏' `shouldReturn` 2

    context "with nonprintable characters" $ do
      it "returns -1" $ do
        wcwidth '\n' `shouldReturn` -1

  describe "parseLocation" $ do
    it "parses a source location" $ do
      parseLocation "Foo.hs:1:1: error: Variable not in scope: foo" `shouldBe` Just ("Foo.hs", 1, 1)
