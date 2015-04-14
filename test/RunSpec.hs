module RunSpec (main, spec) where

import           Helper
import           Data.List

import           Run

main :: IO ()
main = hspec spec

normalizeOutput :: String -> [String]
normalizeOutput = map normalize . lines
  where
    normalize line
      | message `isPrefixOf` line = message ++ "..."
      | otherwise = line
    message = "resource/BrokenSpec.hs:12:7: Not in scope: "

spec :: Spec
spec = do
  describe "trigger" $ do
    it "reloads and runs specs" $ do
      withInterpreter ["resource/Spec.hs"] $ \ghci -> do
        xs <- silence (trigger ghci >> trigger ghci)
        xs `shouldContain` "Ok, modules loaded:"
        xs `shouldContain` "1 example, 0 failures"

    context "with a program that does not compile" $ do
      it "stops after reloading" $ do
        withInterpreter ["resource/BrokenSpec.hs"] $ \ghci -> do
          xs <- silence (trigger ghci >> trigger ghci)
          normalizeOutput xs `shouldBe` [
              "[1 of 1] Compiling Spec             ( resource/BrokenSpec.hs, interpreted )"
            , ""
            , "resource/BrokenSpec.hs:12:7: Not in scope: ..."
            , "Failed, modules loaded: none."
            ]
