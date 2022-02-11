{-# LANGUAGE OverloadedStrings #-}  
module DownloadHtml where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B 
import Network.HTTP.Simple

userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667" 



downloadHtml :: String -> IO B.ByteString
downloadHtml url = do

  request' <- parseRequest url
  let request
        = setRequestMethod "GET"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestHeader "User-Agent" [userAgent]
        $ request'
  getResponseBody <$> httpBS request

