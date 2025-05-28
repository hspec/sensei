module GHC.Diagnostic.Util (
  joinMessageLines
, joinLines
, sortImports
) where

import Imports
import Data.Text qualified as T

import GHC.Diagnostic.Annotated

joinMessageLines :: Text -> Text
joinMessageLines = T.intercalate "\n" . joinLines 1 . T.splitOn "\n"

joinLines :: Int -> [Text] -> [Text]
joinLines required = loop
  where
    hasRequiredLeadingSpaces = T.length >>> (>= required)

    loop :: [Text] -> [Text]
    loop = \ case
      [] -> []
      x : (T.span isSpace -> (hasRequiredLeadingSpaces -> True, y)) : ys -> loop $ mconcat [x, " ", y] : ys
      x : xs -> x : loop xs

sortImports :: RequiredVariable -> [Module] -> [Module]
sortImports variable = sortOn \ case
  (Module module_) -> (qualificationIs, map toModuleComponent components)
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
