{-# LANGUAGE DeriveAnyClass #-}
module GHC.Diagnostic.Type (
  Diagnostic(..)
, Span(..)
, Location(..)
, Severity(..)
, Reason(..)
, parse

, FormatConfig(..)
, format
) where

import           Imports hiding ((<>), empty, unlines)

import           Data.Text qualified as T
import           Data.Yaml (decodeThrow)
import           Text.Printf (printf)
import           Text.PrettyPrint
import           System.Console.ANSI.Codes

data Diagnostic = Diagnostic {
  version :: String
, ghcVersion :: String
, span :: Maybe Span
, severity :: Severity
, code :: Maybe Int
, message :: [Text]
, hints :: [Text]
, reason :: Maybe Reason
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

data Reason = ReasonFlags Flags | ReasonCategory Category
  deriving (Eq, Show, Generic)

instance ToJSON Reason where
  toJSON = \ case
    ReasonFlags value -> toJSON value
    ReasonCategory value -> toJSON value

instance FromJSON Reason where
  parseJSON value =
        ReasonFlags <$> parseJSON value
    <|> ReasonCategory <$> parseJSON value

data Flags = Flags {
  flags :: [String]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Category = Category {
  category :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

parse :: ByteString -> Maybe Diagnostic
parse = fmap removeGhciSpecificHints . decodeThrow

data FormatConfig = FormatConfig {
  showErrorContext :: Bool
, color :: Bool
} deriving (Eq, Show)

format :: FormatConfig -> Diagnostic -> String
format config diagnostic = render $ unlines [
    bold (hang header 4 messageWithHints) <> reset
  , ""
  , ""
  ]
  where
    header :: Doc
    header = span <> colon <+> severity <> colon <+> code <+> reason <> reset <> reset <> setBold

    span :: Doc
    span = case diagnostic.span of
      Nothing -> "<no location info>"
      Just loc -> text loc.file <> colon <> int loc.start.line <> colon <> int loc.start.column

    severity :: Doc
    severity = colorize case diagnostic.severity of
      Warning -> "warning"
      Error -> "error"

    code :: Doc
    code = case diagnostic.code of
      Nothing -> empty
      Just c -> brackets $ "GHC-" <> text (printf "%05d" c)

    reason :: Doc
    reason = case diagnostic.reason of
      Nothing -> empty
      Just r -> bracketed . concatMap formatFlag $ case r of
        ReasonFlags (Flags flags) -> flags
        ReasonCategory (Category category) -> [category]
        where
          formatFlag :: String -> [Doc]
          formatFlag (text -> flag) = map colorize $
            "-W" <> flag : case diagnostic.severity of
              Warning -> []
              Error -> [errorFlag <> flag]

          errorFlag :: Doc
          errorFlag = case r of
            ReasonFlags {} -> "Werror="
            ReasonCategory {} -> "-Werror="

    message :: Doc
    message = bulleted $ map (verbatim . T.stripStart) case config.showErrorContext of
      False -> dropErrorContext diagnostic.message
      True -> diagnostic.message

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

    verbatim :: Text -> Doc
    verbatim = unlines . map (text . unpack) . T.lines

    unlines :: [Doc] -> Doc
    unlines = foldr ($+$) empty

    bracketed :: [Doc] -> Doc
    bracketed xs = "[" <> punctuateComma xs <> "]"

    punctuateComma :: [Doc] -> Doc
    punctuateComma = hcat . punctuate (text ", ")

    colorize :: Doc -> Doc
    colorize doc = bold (
      case diagnostic.severity of
        Warning -> magenta doc
        Error -> red doc
      ) <> setBold

    red :: Doc -> Doc
    red doc = setColor Red <> doc <> reset

    magenta :: Doc -> Doc
    magenta doc = setColor Magenta <> doc <> reset

    bold :: Doc -> Doc
    bold doc = setBold <> doc <> reset

    setColor :: Color -> Doc
    setColor c = ansi (setSGRCode [SetColor Foreground Dull c])

    setBold :: Doc
    setBold = ansi "\ESC[;1m"

    reset :: Doc
    reset = ansi (setSGRCode [Reset])

    ansi :: String -> Doc
    ansi
      | config.color = zeroWidthText
      | otherwise = mempty

removeGhciSpecificHints :: Diagnostic -> Diagnostic
removeGhciSpecificHints diagnostic = diagnostic { hints = map processHint diagnostic.hints }
  where
    isSetLanguageExtension :: Text -> Bool
    isSetLanguageExtension = T.isPrefixOf "  :set -X"

    processHint :: Text -> Text
    processHint input = case T.lines input of
      [hint, "You may enable this language extension in GHCi with:", ghciHint]
        | isSetLanguageExtension ghciHint -> hint
      hint : "You may enable these language extensions in GHCi with:" : ghciHints
        | all isSetLanguageExtension ghciHints -> hint
      _ -> input

dropErrorContext :: [Text] -> [Text]
dropErrorContext = filter \ m -> not $ or $ map ($ m) [
    startsWith "  defined at "
  , startsWith "In an equation for "
  , startsWith "In a stmt of a "
  , startsWith "In the expression: "
  , startsWith "In the Template Haskell quotation "
  ]
  where
    startsWith :: Text -> Text -> Bool
    startsWith = T.isPrefixOf
