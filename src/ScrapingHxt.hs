{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module ScrapingHxt where
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Control.Arrow
import DownloadHtml
import qualified Data.ByteString.Char8 as B

import Control.Exception.Safe
base = "https://apod.nasa.gov/apod"

calYearMonth  :: String -> String
calYearMonth s = "https://apod.nasa.gov/apod/calendar/ca" <> s <> ".html"


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
  let url = calYearMonth ym
  putStrLn $  "access : " <> url
  s <- B.unpack <$> downloadHtml url
  fmap ((base++) . dropWhile (== '.')) . concat . safeTail <$> scraping s extractUrls

scrapingPicUrl url = do
  s <- B.unpack  <$> downloadHtml url
  fmap f . listToMaybe . concat . safeTail <$> scraping s extractPicUrl
  where
    f :: String -> String
    f s = if "http" `isPrefixOf` s then s else base++"/"++s


safeTail :: [a] -> [a]
safeTail (a: as) = as
safeTail [] = []