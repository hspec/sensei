{-# LANGUAGE NoBlockArguments #-}
module Foo where

foo :: IO ()
foo = id do return ()
