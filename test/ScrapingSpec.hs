module ScrapingSpec where

import Test.Hspec

main = hspec spec

spec :: Spec
spec = do
  describe "Scraping test " $ do
    describe "sample test" $ do
      it "one add one is two" $ do
        1+1 `shouldBe` 2