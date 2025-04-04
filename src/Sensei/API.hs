module Sensei.API (
  QuickFixRequest(..)
, quickFix
, deepFix
) where

import Imports

import Network.HTTP.Client
import Data.Aeson qualified as Aeson

import HTTP.Util (makeRequest)

data QuickFixRequest = QuickFixRequest {
  choice :: Maybe Int
} deriving (Eq, Show, Generic)

instance ToJSON QuickFixRequest where
  toJSON = genericKebabEncode

instance FromJSON QuickFixRequest where
  parseJSON = genericKebabDecode

quickFix :: FilePath -> QuickFixRequest -> IO (Bool, LazyByteString)
quickFix dir (RequestBodyLBS . Aeson.encode -> requestBody) = makeRequest dir request
  where
    request :: Request
    request = "http://localhost/quick-fix" { method = "POST", requestBody }

deepFix :: FilePath -> IO (Bool, LazyByteString)
deepFix dir = makeRequest dir request
  where
    request :: Request
    request = "http://localhost/deep-fix" { method = "POST" }
