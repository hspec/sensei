{-# LANGUAGE DeriveAnyClass #-}
module GHC.Diagnostic where

import Prelude hiding (span)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), decode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (fromStrict, toStrict)

data Diagnostic = Diagnostic {
  version :: String
, ghcVersion :: String
, span :: Span
, severity :: Severity
, code :: Maybe Int
, message :: [String]
, hints :: [String]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Span = Span {
  file :: FilePath
, start :: Location
, end :: Location
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Location = Location {
  line :: Int
, column :: Int
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Severity = Warning | Error
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: ByteString -> Maybe Diagnostic
parse = decode . fromStrict

format :: Diagnostic -> ByteString
format diagnostic = build $ loc <> ": " <> severity <> code <> message <> "\n"
  where
    build :: Builder -> ByteString
    build = toStrict . Builder.toLazyByteString

    loc :: Builder
    loc = Builder.stringUtf8 diagnostic.span.file <> ":" <> Builder.intDec start.line <> ":" <> Builder.intDec start.column
      where
        start = diagnostic.span.start

    severity :: Builder
    severity = case diagnostic.severity of
      Warning -> "warning: "
      Error -> "error: "

    code :: Builder
    code = case diagnostic.code of
      Nothing -> ""
      Just c -> "[GHC-" <> Builder.intDec c <> "] "

    message :: Builder
    message = Builder.stringUtf8 (unlines diagnostic.message)
