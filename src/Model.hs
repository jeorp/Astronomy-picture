 {-# LANGUAGE TemplateHaskell #-}
 module Model where

import Control.Lens
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Info = Info 
  {
    _url :: String,
    _description :: String
    
  } deriving(Show, Eq)

makeLenses ''Info

instance FromRow Info where
  fromRow = Info <$> field <*> field
