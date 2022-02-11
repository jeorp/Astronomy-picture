module App where

import Control.Monad
import Scraping
import DownloadContent

import Control.Arrow ((&&&))

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate

import Network.HTTP.Simple
import Control.Exception.Safe

fistYaer = 1995 
fisrtMonth = 6

stepMonth :: (Int, Int) -> (Int, Int)
stepMonth (year, month)
 | month `div` 12 == 0 = (year + 1, 1)
 | otherwise = (year, month + 1)

toYM :: (Integer, Int) -> String
toYM (year, month)
 | length (show year) == 4 && month <= 12 = 
   let m = if month < 10 then '0': show month else show month
   in drop 2 (show year) ++ m
 | otherwise = ""

today :: IO (Integer, Int)
today = f . utctDay <$> getCurrentTime
  where
    f :: Day -> (Integer, Int)
    f = (fst &&& (fst . dayOfYearToMonthAndDay True . snd)) . toOrdinalDate 

errorHandlerSimple = 
  [
    Handler (\(NonContent e) -> print e),
    Handler (\(InvalidUrlException url e) -> print $ url <> " is " <> e)
  ]


donwloadAstronomyPicFromYMIO :: String -> String -> IO ()
donwloadAstronomyPicFromYMIO temp ym = do
  urls <- scrapingUrls ym 
  forM_ urls $ \url -> do 
    picUrl <- scrapingPicUrl url 
    case picUrl of
      Just url -> do
        storeFromUrl temp url `catches` errorHandlerSimple
      Nothing -> pure ()
