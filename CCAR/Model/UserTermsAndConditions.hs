{--License: license.txt --}
module CCAR.Model.UserTermsAndConditions 
where
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J


data CRUD = Create  | Update  | 
	Query | Delete  deriving(Show, Eq, Generic)
instance ToJSON CRUD
instance FromJSON CRUD