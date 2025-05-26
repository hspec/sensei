module Foo where
foo :: [a] -> (Int, [a])
foo = length &&& id
