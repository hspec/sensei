module GHC.Diagnostic.Util (
  joinMessageLines
, sortImports
) where

import Data.Text qualified as T
import Imports

import GHC.Diagnostic.Annotated

joinMessageLines :: Text -> Text
joinMessageLines = T.intercalate "\n" . loop . T.splitOn "\n"
  where
    loop :: [Text] -> [Text]
    loop = \ case
      [] -> []
      x : (T.span isSpace -> (T.null -> False, y)) : ys -> loop $ (x <> " " <> y) : ys
      x : xs -> x : loop xs

sortImports :: RequiredVariable -> [Module] -> [Module]
sortImports variable = case variable.qualification of
  Unqualified -> sort
  Qualified qualification -> sortOn f
    where
      -- f :: Module -> (Down Bool, Down Bool, [ModuleComponent])
      f (Module module_) = (foo, map toModuleComponent moduleComponents)
        where
          foo
            | qualification `elem` moduleComponents = QualificationIsModuleComponent
            | any (T.isPrefixOf qualification) moduleComponents = QualificationIsPrefixOfModuleComponent
            | otherwise = QualificationIsUnrelatedToModuleComponent

          moduleComponents :: [Text]
          moduleComponents = T.splitOn "." module_

data ModuleComponent = ModuleComponent Text | InternalModuleComponent Text
  deriving Eq

instance Ord ModuleComponent where
  compare (ModuleComponent a) (ModuleComponent b) = compare a b
  compare (InternalModuleComponent a) (InternalModuleComponent b) = compare a b
  compare (InternalModuleComponent _) (ModuleComponent _) = GT
  compare (ModuleComponent _) (InternalModuleComponent _) = LT

toModuleComponent :: Text -> ModuleComponent
toModuleComponent name = case name of
  "Internal" -> InternalModuleComponent name
  _ -> ModuleComponent name

data QualificationIs =
    QualificationIsModuleComponent
  | QualificationIsPrefixOfModuleComponent
  | QualificationIsUnrelatedToModuleComponent
  deriving (Eq, Ord)
