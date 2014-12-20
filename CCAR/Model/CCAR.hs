module CCAR.Model.CCAR 
where
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J

data CRUD = Create  | Update CCARId | Query CCARId | Delete CCARId deriving(Show, Eq, Generic)
instance ToJSON CRUD
instance FromJSON CRUD