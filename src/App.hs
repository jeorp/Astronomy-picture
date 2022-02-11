module App where

import Control.Monad
import Scraping
import DownloadContent

import Network.HTTP.Simple
import Control.Exception.Safe

fistYaer = 1995 
fisrtMonth = 6

errorHandlerSimple = 
  [
    Handler (\(NonContent e) -> print e),
    Handler (\(InvalidUrlException url e) -> print $ url <> " is " <> e)
  ]

donwloadAstronomyPicFromYMIO :: String -> Int -> IO ()
donwloadAstronomyPicFromYMIO temp ym = do
  urls <- scrapingUrls ym 
  forM_ urls $ \url -> do 
    picUrl <- scrapingPicUrl url 
    case picUrl of
      Just url -> do
        storeFromUrl temp url `catches` errorHandlerSimple
      Nothing -> pure ()
