module GHC.Diagnostic.Annotated where

import GHC.Diagnostic.Type

data Annotated = Annotated {
  diagnostic :: Diagnostic
, annotation :: Maybe Annotation
} deriving (Eq, Show)

data Annotation =
    VariableNotInScopeAnnotation VariableNotInScope
  | RedundantImportAnnotation RedundantImport
  | MissingExtensionAnnotation MissingExtension
  deriving (Eq, Show)

data VariableNotInScope = VariableNotInScope {
  suggestions :: [String]
} deriving (Eq, Show)


data RedundantImport = RedundantImport {
} deriving (Eq, Show)


data MissingExtension = MissingExtension {
  extensions :: [String]
} deriving (Eq, Show)
