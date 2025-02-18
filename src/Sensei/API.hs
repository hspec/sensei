module Sensei.API (
  QuickFixRequest(..)
, quickFix
) where

import Imports

import Network.HTTP.Client

import HTTP
import Client

import Data.Aeson qualified as Aeson

quickFix :: FilePath -> QuickFixRequest -> IO (Bool, LazyByteString)
quickFix dir (RequestBodyLBS . Aeson.encode -> requestBody) = makeRequest dir url
  where
    url :: Request
    url = "http://localhost/quick-fix" { method = "POST", requestBody }
