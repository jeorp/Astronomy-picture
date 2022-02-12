module ScrapingSpec where

import Test.Hspec
import Scraping
import DownloadHtml

main = hspec spec



spec :: Spec
spec = do
  describe "Scraping test " $ do
    
    describe "scraping urls" $ do
      it "first url is ..." $ do
        head <$> scrapingUrls "1601" `shouldReturn` "https://apod.nasa.gov/apod/ap160101.html"

    describe "scraping pic url" $ do
      it "160103 pic url is ..." $ do
        let sampleUrl = "https://apod.nasa.gov/apod/ap160103.html"
        scrapingPicUrl sampleUrl `shouldReturn` Just "https://apod.nasa.gov/apod/image/1601/aurora_vetter_2000.jpg"