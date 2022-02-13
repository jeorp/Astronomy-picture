module ScrapingConduit where

import Scraping 
import Control.Concurrent.Async
import Conduit

ymFromTo :: (Integer, Int) -> (Integer, Int) -> [(Integer, Int)]
ymFromTo from to
 | from <= to = from : ymFromTo (stepMonth from) to
 | otherwise = []

downloadPicFromYMStream :: String -> (Integer, Int) -> (Integer, Int) -> IO ()
downloadPicFromYMStream temp from to = runConduit
  $ yieldMany (ymFromTo from to)
  .| mapMC getPicUrlAtYM
  .| mapM_C (mapConcurrently_ (downloadPicWithHandlerSimple temp))

downloadPicAtYStream :: String -> Integer -> IO ()
downloadPicAtYStream temp year = downloadPicFromYMStream temp (year, 1) (year, 12)

donwloadAstronomyAllPicStreamM :: String -> IO ()
donwloadAstronomyAllPicStreamM temp = do
  now <- today
  let
    firstYear = fst firstYM
    latterYear = fst now 
    years = [firstYear .. latterYear]
  mapM_ (downloadPicAtYStream temp) years