module Foo where
foo :: FilePath -> IO String
foo name = do
  r <- _ name
  return r
