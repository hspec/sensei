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
  | IgnoreWarning Text
  | RemoveImport
  | ReplaceImport Text Text
  | CreateModule FilePath Text
  | ReplaceName Text Text
  | ImportName Module Qualification Text
  | AddArgument Text
  | AddPatterns [Text]
  deriving (Eq, Show)

data Annotation =
    RedundantImport
  | UnknownImport Text [Text]
  | VariableNotInScope RequiredVariable
  | TermLevelUseOfTypeConstructor RequiredVariable
  | TypeNotInScope Qualification Text
  | FoundHole Text Type [HoleFit]
  | FoundTypeHole Text Text
  | NonExhaustivePatternMatch Text [Text]
  deriving (Eq, Show)

data RequiredVariable = RequiredVariable {
  qualification :: Qualification
, name :: Text
, type_ :: TypeSignature
} deriving (Eq, Show)

data Qualification = Unqualified | Qualified Text
  deriving (Eq, Show)

data HoleFit = HoleFit {
  name :: Text
, type_ :: TypeSignature
} deriving (Eq, Show)

data TypeSignature = NoTypeSignature | TypeSignature Type
  deriving (Eq, Ord, Show)

data Module = Module {
  package :: Package
, name :: Text
} deriving (Eq, Ord, Show)

data Package = Package {
  type_ :: PackageType
, name :: Text
} deriving (Eq, Ord, Show)

data PackageType = CurrentPackage | DirectDependency | TransitiveDependency
  deriving (Eq, Ord, Show)

newtype Type = Type Text
  deriving newtype (Eq, Ord, Show, IsString)
