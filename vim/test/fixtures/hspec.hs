import Test.Hspec

main :: IO ()
main = hspec $ do
  it "foo" $ do
    "foo" `shouldBe` "bar"
