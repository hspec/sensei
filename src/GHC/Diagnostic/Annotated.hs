module GHC.Diagnostic.Annotated where

import Imports

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
  name :: RequiredVariable
, suggestions :: [SuggestIdentifier]
} deriving (Eq, Show)


data RequiredVariable = RequiredVariable {
  qualification :: Qualification
, name :: Text
, type_ :: Maybe Text
} deriving (Eq, Show)

data Qualification = Unqualified | Qualified Text
  deriving (Eq, Show)

instance IsString Qualification where
  fromString = Qualified . fromString

{-
requiredVariable name = case T.breakEndEnd "." of
  ("", n) -> RequiredVariable Nothing n Nothing
  (qualification, n) -> RequiredVariable (Just qualification) n Nothing
  -}

instance IsString RequiredVariable where
  fromString name = RequiredVariable Unqualified (fromString name) Nothing

data RedundantImport = RedundantImport {
} deriving (Eq, Show)


data MissingExtension = MissingExtension {
  extensions :: [String]
} deriving (Eq, Show)

data Identifier = Identifier {
  module_ :: Module
, name :: String
} deriving (Eq, Show)

data SuggestIdentifier =
    SuggestIdentifier Identifier
  | IdentifierInScope Text
  deriving (Eq, Show)

newtype Module = Module Text
  deriving newtype (Eq, Ord, Show, IsString)
