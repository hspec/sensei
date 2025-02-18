module Sensei.API (
  QuickFixRequest(..)
, quickFix
) where

import Imports

import Network.HTTP.Client
import Data.Aeson qualified as Aeson

import HTTP.Util (makeRequest)

data QuickFixRequest = QuickFixRequest {
  deepSeek :: Maybe Bool
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
