{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Scraping where
import Data.Char
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Control.Arrow
import DonwloadHtml

baseYearMonth  :: Int -> String
baseYearMonth s 
 | length (show s) == 4 = "https://apod.nasa.gov/apod/calendar/ca" <> show s <> ".html"
 | otherwise = "error"

sampleUrl = "https://apod.nasa.gov/apod/ap160103.html"

parseHTML = readString 
  [ withValidate no,
    withParseHTML yes,
    withWarnings no
  ]

scraping body parser = runX (parseHTML body >>> parser)


atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper


