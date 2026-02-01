module Imports (module Imports) where

import "base" Prelude as Imports hiding (span, head)
import "base" Control.Arrow as Imports ((>>>), (&&&))
import "base" Control.Monad as Imports
import "base" Control.Applicative as Imports
import "base" Data.Function as Imports (fix)
import "base" Data.Functor as Imports ((<&>), ($>))
import "base" Data.Foldable as Imports
import "base" Data.Traversable as Imports
import "base" Data.Bifunctor as Imports
import "base" Data.Char as Imports
import "base" Data.Either as Imports
import "base" Data.List as Imports hiding (insert, span, head)
import "base" Data.Maybe as Imports
import "base" Data.String as Imports
import "base" Data.Tuple as Imports

import        Data.ByteString as Imports (ByteString)
import        Data.Text as Imports (Text, pack, unpack)
import        Data.Set as Imports (Set)
import        System.FilePath as Imports hiding (addExtension, combine)

pass :: Applicative m => m ()
pass = pure ()

head :: [a] -> Maybe a
head = listToMaybe

data GHC =
    GHC_910
  | GHC_912
  | GHC_914
  deriving (Eq, Show, Ord, Bounded)

requiredFor :: GHC -> a -> a
requiredFor _ = id
{-# INLINE requiredFor #-}
