module Foo where
import Prelude hiding (fail)
foo :: FilePath -> IO String
foo name = do
  r <- _ name
  return r
