module Foo where
import Prelude (Int)
import qualified Data.Maybe as Some.Very.Long.Hierarchical.Module.Name

foo :: Some.Very.Long.Hierarchical.Module.Name.Maybe Int -> Int
foo x = case x of
