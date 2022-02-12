module App where

import Control.Monad
import Scraping
import DownloadContent

import Control.Arrow ((&&&))

import Data.Time.Clock
import Data.Time.Calendar

import Model

import Network.HTTP.Simple
import Control.Exception.Safe

firstYM :: (Integer, Int)
firstYM = (1995, 6) 

stepMonth :: (Integer, Int) -> (Integer, Int)
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
    f d = let (y, m, _) = toGregorian d in (y, m)

errorHandlerSimple = 
  [
    Handler (\(NonContent e) -> putStrLn $ "erorr : " <> e <> " is not picture url"),
    Handler (\(InvalidUrlException url e) -> print $ url <> " is " <> e)
  ]


getInfoFromYM :: String -> IO Info
getInfoFromYM ym = undefined

donwloadAstronomyPicFromYMIO :: String -> String -> IO ()
donwloadAstronomyPicFromYMIO temp ym = do
  urls <- scrapingUrls ym 
  forM_ urls $ \url -> do 
    picUrl <- scrapingPicUrl url 
    case picUrl of
      Just url -> do
        storeFromUrl temp url `catches` errorHandlerSimple
      Nothing -> pure ()

donwloadAstronomyPicSimpleIO :: String -> IO ()
donwloadAstronomyPicSimpleIO temp = loop firstYM
  where
    loop :: (Integer, Int) -> IO ()
    loop ym = do
      now <- today
      if ym <= now
        then 
          donwloadAstronomyPicFromYMIO temp (toYM ym) >> loop (stepMonth ym)
        else 
          putStrLn "finish !"
      
