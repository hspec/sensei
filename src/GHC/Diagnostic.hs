{-# LANGUAGE DeriveAnyClass #-}
module GHC.Diagnostic (
  Diagnostic(..)
, Span(..)
, Location(..)
, Severity(..)
, parse
, format
) where

import           Prelude hiding ((<>), span, unlines)
import           Imports hiding (empty, unlines)
import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON(..), FromJSON(..), decode)
import           Data.ByteString.Lazy (fromStrict)
import           Text.PrettyPrint

data Diagnostic = Diagnostic {
  version :: String
, ghcVersion :: String
, span :: Maybe Span
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
format diagnostic = encodeUtf8 . render $ unlines [
    hang header 4 messageWithHints
  , ""
  , ""
  ]
  where
    header :: Doc
    header = span <> colon <+> severity <> colon <+> code

    span :: Doc
    span = case diagnostic.span of
      Nothing -> "<no location info>"
      Just loc -> text loc.file <> colon <> int loc.start.line <> colon <> int loc.start.column

    severity :: Doc
    severity = case diagnostic.severity of
      Warning -> "warning"
      Error -> "error"

    code :: Doc
    code = case diagnostic.code of
      Nothing -> empty
      Just c -> brackets $ "GHC-" <> int c

    message :: Doc
    message = bulleted $ map verbatim diagnostic.message

    hints :: [Doc]
    hints = map verbatim diagnostic.hints

    messageWithHints :: Doc
    messageWithHints = case hints of
      []  -> message
      [h] -> message $$ hang (text "Suggested fix:") 2 h
      hs  -> message $$ hang (text "Suggested fixes:") 2 (bulleted hs)

    bulleted :: [Doc] -> Doc
    bulleted = \ case
      [] -> empty
      [doc] -> doc
      docs -> vcat $ map (char '•' <+>) docs

    verbatim :: String -> Doc
    verbatim = unlines . map text . lines

    unlines :: [Doc] -> Doc
    unlines = foldr ($+$) empty