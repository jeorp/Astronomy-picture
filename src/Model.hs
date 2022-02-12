 module Model where
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Info = Info 
  {
    _url :: String,
    _description :: String
    
  } deriving(Show, Eq)

instance FromRow Info where
  fromRow = Info <$> field <*> field
