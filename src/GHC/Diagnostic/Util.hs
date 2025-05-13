module GHC.Diagnostic.Util (
  joinMessageLines
, sortImports
) where

import Imports
import Data.Text qualified as T

import GHC.Diagnostic.Annotated

joinMessageLines :: Text -> Text
joinMessageLines = T.intercalate "\n" . loop . T.splitOn "\n"
  where
    loop :: [Text] -> [Text]
    loop = \ case
      [] -> []
      x : (T.span isSpace -> (T.null -> False, y)) : ys -> loop $ mconcat [x, " ", y] : ys
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
