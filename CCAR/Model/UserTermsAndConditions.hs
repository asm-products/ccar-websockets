module CCAR.Model.UserTermsAndConditions 
where
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J


data CRUD = Create  | Update TermsAndConditionsId | 
	Query TermsAndConditionsId | Delete TermsAndConditionsId deriving(Show, Eq, Generic)
instance ToJSON CRUD
instance FromJSON CRUD