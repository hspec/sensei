module Foo where
data Bar = Bar (IO ())
  deriving Eq
