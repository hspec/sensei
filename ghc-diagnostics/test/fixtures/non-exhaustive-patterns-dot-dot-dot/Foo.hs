module Foo where
import GHC.IO.Exception

foo :: IOErrorType -> Int
foo x = case x of
  AlreadyExists -> undefined
