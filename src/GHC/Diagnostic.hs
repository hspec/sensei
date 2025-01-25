module GHC.Diagnostic (
  module Diagnostic
, Action(..)
, analyze
, apply
) where

import           Imports

import           System.IO
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString.Builder (hPutBuilder)

import           GHC.Diagnostic.Type as Diagnostic

data Action = AddExtension FilePath Text
  deriving (Eq, Show)

analyze :: Diagnostic -> Maybe Action
analyze diagnostic = listToMaybe $ mapMaybe analyzeHint diagnostic.hints
  where
    analyzeHint :: String -> Maybe Action
    analyzeHint (T.pack -> hint) =
          perhapsYouIntendedToUse
      <|> enableAnyOfTheFollowingExtensions
      where
        perhapsYouIntendedToUse :: Maybe Action
        perhapsYouIntendedToUse = do
          AddExtension . (.file) <$> diagnostic.span <*> T.stripPrefix "Perhaps you intended to use " hint

        enableAnyOfTheFollowingExtensions :: Maybe Action
        enableAnyOfTheFollowingExtensions = do
          file <- (.file) <$> diagnostic.span
          T.stripPrefix "Enable any of the following extensions: " hint
            >>= listToMaybe . reverse . map (AddExtension file) . T.splitOn ", "

apply :: Action -> IO ()
apply = \ case
  AddExtension file name -> do
    old <- B.readFile file
    withFile file WriteMode $ \ h -> do
      hPutBuilder h $ "{-# LANGUAGE " <> T.encodeUtf8Builder name <> " #-}\n"
      B.hPutStr h old
