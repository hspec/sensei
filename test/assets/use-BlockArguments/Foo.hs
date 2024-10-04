{-# LANGUAGE NoBlockArguments #-}
module BlockArguments.Foo where

foo :: IO ()
foo = id do return ()
