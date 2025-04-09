module Sensei.API (
  quickFix
, deepFix

, QuickFixRequest(..)

, DeepFixRequest(..)
, Instructions(..)
, Span(..)
, Location(..)
) where

import Imports

import Network.HTTP.Client
import Data.Aeson qualified as Aeson

import GHC.Diagnostic.Type (Span(..), Location(..))
import HTTP.Util (makeRequest)

data QuickFixRequest = QuickFixRequest {
  choice :: Maybe Int
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions QuickFixRequest)

quickFix :: FilePath -> QuickFixRequest -> IO (Bool, LazyByteString)
quickFix = post "/quick-fix"

data DeepFixRequest = DeepFixRequest {
  instructions :: Maybe Instructions
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions DeepFixRequest)

data Instructions = Instructions {
  span :: Span
, instructions :: Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (KebabOptions Instructions)

deepFix :: FilePath -> DeepFixRequest -> IO (Bool, LazyByteString)
deepFix = post "/deep-fix"

post :: ToJSON a => String -> FilePath -> a -> IO (Bool, LazyByteString)
post endpoint dir (RequestBodyLBS . Aeson.encode -> requestBody) = makeRequest dir request
  where
    request :: Request
    request = (fromString $ "http://localhost" <> endpoint) { method = "POST", requestBody }
