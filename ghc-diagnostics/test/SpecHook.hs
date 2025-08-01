module SpecHook (hook) where

import Imports

import Test.Hspec
import GHC.Conc

hook :: Spec -> Spec
hook spec = runIO (getNumProcessors >>= setNumCapabilities) >> parallel spec
