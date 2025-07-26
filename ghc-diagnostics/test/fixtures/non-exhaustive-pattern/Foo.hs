module Foo where
foo :: Maybe Int -> Int
foo x = case x of
  Just n -> n
