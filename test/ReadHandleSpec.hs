{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module ReadHandleSpec (spec) where

import           Helper
import           Test.QuickCheck
import qualified Data.ByteString as B

import           ReadHandle

chunkByteString :: (Int, Int) -> ByteString -> Gen [ByteString]
chunkByteString size = go
  where
    go "" = return []
    go xs = do
      n <- chooseInt size
      let (chunk, rest) = B.splitAt n xs
      (chunk :) <$> go rest

fakeHandle :: [ByteString] -> IO ReadHandle
fakeHandle chunks = ReadHandle <$> stubAction chunks <*> newEmptyBuffer

data ChunkSizes = SmallChunks | BigChunks

withRandomChunkSizes :: [ByteString] -> (ReadHandle -> Expectation) -> Property
withRandomChunkSizes (mconcat -> input) action = property $ do
  chunkSizes <- elements [SmallChunks, BigChunks]
  let
    maxChunkSize = case chunkSizes of
      SmallChunks -> 4
      BigChunks -> B.length input

  chunks <- chunkByteString (1, maxChunkSize) input
  return $ fakeHandle chunks >>= action

partialMarker :: ByteString
partialMarker = B.take 5 marker

spec :: Spec
spec = do
  describe "getResult" $ do
    context "with a single result" $ do
      let input = ["foo", "bar", "baz", marker]

      it "returns result" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle input
          getResult h echo `shouldReturn` "foobarbaz"
        `shouldReturn` ["foo", "bar", "baz"]

      context "with chunks of arbitrary size" $ do
        it "returns result" $ do
          withRandomChunkSizes input $ \ h -> do
            fmap mconcat . withSpy $ \ echo -> do
              getResult h echo `shouldReturn` "foobarbaz"
            `shouldReturn` "foobarbaz"

    context "with multiple results" $ do
      let input = ["foo", marker, "bar", marker, "baz", marker]

      it "returns one result at a time" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle input
          getResult h echo `shouldReturn` "foo"
          getResult h echo `shouldReturn` "bar"
          getResult h echo `shouldReturn` "baz"
        `shouldReturn` ["foo", "bar", "baz"]

      context "with chunks of arbitrary size" $ do
        it "returns one result at a time" $ do
          withRandomChunkSizes input $ \ h -> do
            fmap mconcat . withSpy $ \ echo -> do
              getResult h echo `shouldReturn` "foo"
              getResult h echo `shouldReturn` "bar"
              getResult h echo `shouldReturn` "baz"
            `shouldReturn` "foobarbaz"

    context "when a chunk that contains a marker ends with a partial marker" $ do
      it "correctly gives the marker precedence over the partial marker" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle ["foo" <> marker <> "bar" <> partialMarker, ""]
          getResult h echo `shouldReturn` "foo"
          getResult h echo `shouldReturn` ("bar" <> partialMarker)
        `shouldReturn` ["foo", "bar", partialMarker]

    context "on EOF" $ do
      it "returns all remaining input" $ do
        withSpy $ \ echo -> do
          h <- fakeHandle ["foo", "bar", "baz", ""]
          getResult h echo `shouldReturn` "foobarbaz"
        `shouldReturn` ["foo", "bar", "baz"]

      context "with a partialMarker at the end" $ do
        it "includes the partial marker in the output" $ do
          withSpy $ \ echo -> do
            h <- fakeHandle ["foo", "bar", "baz", partialMarker, ""]
            getResult h echo `shouldReturn` ("foobarbaz" <> partialMarker)
          `shouldReturn` ["foo", "bar", "baz", partialMarker]

      context "after a marker" $ do
        it "returns all remaining input" $ do
          withSpy $ \ echo -> do
            h <- fakeHandle ["foo", "bar", "baz", marker, "qux", ""]
            getResult h echo `shouldReturn` "foobarbaz"
            getResult h echo `shouldReturn` "qux"
          `shouldReturn` ["foo", "bar", "baz", "qux"]
