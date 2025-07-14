module GHC.Diagnostic.Util (
  joinMessageLines
, joinLines

, Name(..)
, NameSpace(..)
, sortImports
) where

import Imports
import Data.Text qualified as T
import Data.Set (isSubsetOf)
import Data.Set qualified as Set

import GHC.Diagnostic.Annotated

joinMessageLines :: Text -> Text
joinMessageLines = T.intercalate "\n" . joinLines 1 . T.splitOn "\n"

joinLines :: Int -> [Text] -> [Text]
joinLines required = loop
  where
    hasRequiredLeadingSpaces :: Text -> Bool
    hasRequiredLeadingSpaces = T.length >>> (>= required)

    loop :: [Text] -> [Text]
    loop = \ case
      [] -> []
      x : (T.span isSpace -> (hasRequiredLeadingSpaces -> True, y)) : ys -> loop $ mconcat [x, " ", y] : ys
      x : xs -> x : loop xs

data Name = Name {
  nameSpace :: NameSpace
, name :: Text
} deriving (Eq, Show, Ord)

data NameSpace =
    VariableName
  | TypeName
  deriving (Eq, Show, Ord)

sortImports :: Qualification -> Name -> (a -> Module) -> [a] -> [a]
sortImports qual required f = sortOn $ f >>> \ case
  (Module package module_) -> (
      package.type_ == TransitiveDependency
    , type_name_is_a_module_name_component
    , qualification_is_related_to_module_name
    , package.type_
    , deprioritize_ghc_modules_from_base
    , moduleComponents
    )
    where
      type_name_is_a_module_name_component :: Bool
      type_name_is_a_module_name_component = case required.nameSpace of
        VariableName -> False
        TypeName -> not $ required.name `Set.member` componentsSet

      qualification_is_related_to_module_name :: QualificationIs
      qualification_is_related_to_module_name = case qual of
          Unqualified -> QualificationIsUnrelatedToModuleName
          Qualified qualification
            | qualification `elem` components -> QualificationIsModuleComponent
            | any (T.isPrefixOf qualification) components -> QualificationIsPrefixOfModuleComponent
            | otherwise -> QualificationIsUnrelatedToModuleName

      deprioritize_ghc_modules_from_base :: Bool
      deprioritize_ghc_modules_from_base =
        package.name == "base" && head components == Just "GHC"

      components :: [Text]
      components = T.splitOn "." module_

      componentsSet :: Set Text
      componentsSet = Set.fromList components

      moduleComponents :: ModuleComponents
      moduleComponents = ModuleComponents (map toModuleComponent components) componentsSet

data ModuleComponents = ModuleComponents {
  components :: [ModuleComponent]
, asSet :: Set Text
}

instance Eq ModuleComponents where
  a == b = a.components == b.components

instance Ord ModuleComponents where
  compare a b
    | a == b = EQ
    | otherwise = case a.asSet `isSubsetOf` b.asSet of
        True -> LT
        False -> case b.asSet `isSubsetOf` a.asSet of
          True -> GT
          False -> compare a.components b.components

data QualificationIs =
    QualificationIsModuleComponent
  | QualificationIsPrefixOfModuleComponent
  | QualificationIsUnrelatedToModuleName
  deriving (Eq, Ord)

data ModuleComponent = ModuleComponent Text | InternalModuleComponent Text
  deriving (Eq, Ord)

toModuleComponent :: Text -> ModuleComponent
toModuleComponent name = case name of
  "Internal" -> InternalModuleComponent name
  _ -> ModuleComponent name
