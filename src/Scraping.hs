{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Scraping where
import Data.Char
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Control.Arrow
import DownloadHtml
import qualified Data.ByteString.Char8 as B

base = "https://apod.nasa.gov/apod"

calYearMonth  :: Int -> String
calYearMonth s 
 | length (show s) == 4 = "https://apod.nasa.gov/apod/calendar/ca" <> show s <> ".html"
 | otherwise = "error"


parseHTML = readString 
  [ withValidate no,
    withParseHTML yes,
    withWarnings no
  ]

scraping body parser = runX (parseHTML body >>> parser)


atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

onA = proc r -> do
  a <- atTagCase "a" -< r
  getAttrValue "href" -< a

extractUrls =
  atTagCase "tr"
  >>> proc r -> do
    listA onA -< r 

extractPicUrl =
  atTagCase "p" 
  >>> proc r -> do
    listA onA -< r


scrapingUrls ym = do
  s <- B.unpack <$> downloadHtml (calYearMonth ym)
  concat . tail <$> scraping s extractUrls

scrapingPicUrl url = do
  s <- B.unpack  <$> downloadHtml url
  concat . tail <$> scraping s extractPicUrl