module GHC.Diagnostic.Annotated where

import Imports

import GHC.Diagnostic.Type

data Annotated = Annotated {
  diagnostic :: Diagnostic
, annotation :: Maybe Annotation
, solutions :: [Solution]
} deriving (Eq, Show)

data Solution =
    EnableExtension Text
  | RemoveImport
  | UseName Text
  | ImportName Module Qualification Text
  deriving (Eq, Show)

data Annotation =
    RedundantImport
  | NotInScope RequiredVariable
  deriving (Eq, Show)

data RequiredVariable = RequiredVariable {
  qualification :: Qualification
, name :: Text
, type_ :: Maybe Text
} deriving (Eq, Show)

instance IsString RequiredVariable where
  fromString name = RequiredVariable Unqualified (fromString name) Nothing

data Qualification = Unqualified | Qualified Text
  deriving (Eq, Show)

instance IsString Qualification where
  fromString = Qualified . fromString

newtype Module = Module Text
  deriving newtype (Eq, Ord, Show, IsString)
