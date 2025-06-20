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
  | FoundHole Type [HoleFit]
  deriving (Eq, Show)

data RequiredVariable = RequiredVariable {
  qualification :: Qualification
, name :: Text
, type_ :: TypeSignature
} deriving (Eq, Show)

instance IsString RequiredVariable where
  fromString name = RequiredVariable Unqualified (fromString name) NoTypeSignature

data Qualification = Unqualified | Qualified Text
  deriving (Eq, Show)

instance IsString Qualification where
  fromString = Qualified . fromString

data HoleFit = HoleFit {
  name :: Text
, type_ :: TypeSignature
} deriving (Eq, Show)

data TypeSignature = NoTypeSignature | TypeSignature Type
  deriving (Eq, Ord, Show)

instance IsString TypeSignature where
  fromString = TypeSignature . fromString

newtype Module = Module Text
  deriving newtype (Eq, Ord, Show, IsString)

newtype Type = Type Text
  deriving newtype (Eq, Ord, Show, IsString)
