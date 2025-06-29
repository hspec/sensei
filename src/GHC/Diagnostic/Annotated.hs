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
  | VariableNotInScope RequiredVariable
  | TypeNotInScope Qualification Text
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

data Module = Module {
  package :: Text
, name :: Text
} deriving (Eq, Ord, Show)

instance IsString Module where
  fromString = Module "base" . fromString

newtype Type = Type Text
  deriving newtype (Eq, Ord, Show, IsString)
