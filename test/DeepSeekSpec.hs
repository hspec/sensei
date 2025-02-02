module DeepSeekSpec (spec) where

import           Helper

import           DeepSeek

spec :: Spec
spec = do
  describe "extractPatch" $ do
    it "extracts patch" $ do
      let
        err :: Diagnostic
        err = (diagnostic Error) {
          span = Just Span {
            file = "src/DeepSeek/Types.hs"
          , start = Location 0 0
          , end = Location 0 0
          }
        }
        input :: String
        input = unlines [
            "```diff"
          , "--- a/src/DeepSeek/Types.hs"
          , "+++ b/src/DeepSeek/Types.hs"
          , "@@ -11,7 +11,7 @@"
          , " import GHC.Generics (Generic)"
          , " import Data.Aeson (FromJSON, ToJSON)"
          , " import Data.Text (Text)"
          , "-import Data.Time.Clock.POSIX (POSIXTime)"
          , "+import Data.Time.Clock.POSIX ()"
          , ""
          , " data Request = Request {"
          , "   model :: String"
          , "```"
          ]
      extractPatch err input `shouldBe` Just Patch {
          strip = 1
        , diff = unlines [
            "--- a/src/DeepSeek/Types.hs"
          , "+++ b/src/DeepSeek/Types.hs"
          , "@@ -11,7 +11,7 @@"
          , " import GHC.Generics (Generic)"
          , " import Data.Aeson (FromJSON, ToJSON)"
          , " import Data.Text (Text)"
          , "-import Data.Time.Clock.POSIX (POSIXTime)"
          , "+import Data.Time.Clock.POSIX ()"
          , ""
          , " data Request = Request {"
          , "   model :: String"
          ]
        }
