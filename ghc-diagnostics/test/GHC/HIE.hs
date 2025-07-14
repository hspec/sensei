module GHC.HIE where

import Imports

import Data.Coerce

import GHC.Unit.Types qualified
import GHC.Types.Avail
import GHC.Types.Name hiding (Name)
import GHC.Types.Name qualified as GHC
import Language.Haskell.Syntax.Module.Name

import Data.Text.Internal (text)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import GHC.Data.FastString (FastString, fs_sbs)

import "ghc-hie" GHC.Iface.Ext.Types as HIE
import GHC.Diagnostic (Name(..), NameSpace(..), ProvidedBy(..))
import GHC.Diagnostic.Annotated

import System.Directory (listDirectory, doesDirectoryExist)

hieExports :: Package -> HieFile -> [(Name, ProvidedBy)]
hieExports package hieFile = concatMap allNames hieFile.hie_exports
  where
    module_ :: Module
    module_ = fromModuleName hieFile.hie_module.moduleName

    fromName :: GHC.Name -> Name
    fromName name
      | nameNameSpace name == tcClsName = Name TypeName (ghcNameAsText name)
      | otherwise = Name VariableName (ghcNameAsText name)

    ghcNameAsText :: GHC.Name -> Text
    ghcNameAsText = unpackFS . occNameFS . nameOccName

    fromModuleName :: ModuleName -> Module
    fromModuleName = Module package . unpackFS . coerce

    available :: Maybe Type -> GHC.Name -> (Name, ProvidedBy)
    available type_ name = (fromName name, ProvidedBy module_ type_)

    allNames :: AvailInfo -> [(Name, ProvidedBy)]
    allNames = \ case
      Avail name -> [available Nothing name]
      AvailTC type_ names -> map (available (Just t)) names
        where
          t = Type (ghcNameAsText type_)

unpackFS :: FastString -> Text
unpackFS = unsafeShortByteStringAsText . fs_sbs

unsafeShortByteStringAsText :: ShortByteString -> Text
unsafeShortByteStringAsText bs = text bs.unShortByteString 0 (ShortByteString.length bs)

listHieFiles :: FilePath -> IO [FilePath]
listHieFiles = go
  where
    go :: String -> IO [String]
    go dir = do
      paths <- map (dir </>) <$> listDirectory dir
      let hieFiles = filter (takeExtension >>> (== ".hie")) paths
      (hieFiles ++) . concat <$> (filterM doesDirectoryExist paths >>= mapM go)
