{-# LANGUAGE LambdaCase #-}
module Scraping where

import Control.Monad
import ScrapingHxt
import DownloadContent

import Control.Arrow ((&&&))
import Control.Concurrent.Async

import Data.Time.Clock
import Data.Time.Calendar

import Data.Maybe (catMaybes)
import Data.DList

import Network.HTTP.Simple
import Control.Exception.Safe



firstYM :: (Integer, Int)
firstYM = (1995, 6) -- First post is from 1995 June. 

stepMonth :: (Integer, Int) -> (Integer, Int)
stepMonth (year, month)
 | month `mod` 12 == 0 = (year + 1, 1)
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


errorHandlerSimple :: [Handler IO ()]
errorHandlerSimple = 
  [
    Handler (\(NonContent e) -> putStrLn $ "erorr : " <> e <> " is not picture url"),
    Handler (\(InvalidUrlException url e) -> putStrLn $ url <> " is " <> e)
  ]


getPicUrlAtYM :: (Integer, Int) -> IO [String]
getPicUrlAtYM ym = scrapingUrls (toYM ym) >>= fmap catMaybes . traverse scrapingPicUrl

getPicUrlFromYM :: (Integer, Int) -> (Integer, Int) -> IO [String]
getPicUrlFromYM from to = flip apply [] <$> loop from empty
  where
    loop :: (Integer, Int) -> DList String -> IO (DList String)
    loop ym dl 
     | ym <= to = do  
          xs <- getPicUrlAtYM ym
          loop (stepMonth ym) (append (fromList xs) dl)
     | otherwise =  pure dl

getAllPicUrlSingle :: IO [String]
getAllPicUrlSingle = today >>= getPicUrlFromYM firstYM

getPicUrlAtY :: Integer -> IO [String]
getPicUrlAtY year = getPicUrlFromYM (year, 1) (year, 12)

getAllPicUrlEachYear :: IO [[String]]
getAllPicUrlEachYear = do
  now <- today
  let
    firstYear = fst firstYM
    latterYear = fst now 
    years = [firstYear .. latterYear]
  mapConcurrently getPicUrlAtY years

downloadPicWithHandlerSimple :: String -> String -> IO ()
downloadPicWithHandlerSimple temp url = storeFromUrl temp url `catches` errorHandlerSimple

donwloadAstronomyPicAtYM :: String -> (Integer, Int) -> IO ()
donwloadAstronomyPicAtYM temp ym = getPicUrlAtYM ym >>= mapM_ (downloadPicWithHandlerSimple temp)

      
donwloadAstronomyAllPic :: String -> IO ()
donwloadAstronomyAllPic temp = getAllPicUrlEachYear >>= mapConcurrently_ donwloadEachYear
  where
    donwloadEachYear :: [String] -> IO ()
    donwloadEachYear = mapM_ (downloadPicWithHandlerSimple temp)

