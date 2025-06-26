module GHC.Diagnostic.Util (
  joinMessageLines
, joinLines
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

sortImports :: RequiredVariable -> (a -> Module) -> [a] -> [a]
sortImports variable f = sortOn $ f >>> \ case
  (Module module_) -> (qualificationIs, moduleComponents components)
    where
      qualificationIs :: QualificationIs
      qualificationIs = case variable.qualification of
          Unqualified -> QualificationIsUnrelatedToModuleName
          Qualified qualification
            | qualification `elem` components -> QualificationIsModuleComponent
            | any (T.isPrefixOf qualification) components -> QualificationIsPrefixOfModuleComponent
            | otherwise -> QualificationIsUnrelatedToModuleName

      components :: [Text]
      components = T.splitOn "." module_

data ModuleComponents = ModuleComponents {
  components :: [ModuleComponent]
, asSet :: Set Text
}

instance Eq ModuleComponents where
  a == b = a.components == b.components

moduleComponents :: [Text] -> ModuleComponents
moduleComponents components = ModuleComponents (map toModuleComponent components) (Set.fromList components)

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
