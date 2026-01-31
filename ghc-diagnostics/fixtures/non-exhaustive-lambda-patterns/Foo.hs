module Foo where
data Foo = Foo | Bar String | Baz Int String

foo :: Foo -> Int
foo = \ case
