module CCAR.Model.Person 
where
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J


data CRUD = Create  | Update PersonId | Query PersonId | Delete PersonId deriving(Show, Eq, Generic)
instance ToJSON CRUD
instance FromJSON CRUD