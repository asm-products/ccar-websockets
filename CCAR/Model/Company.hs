module CCAR.Model.Company
	( promoteToChatMinder
	, revokeChatMinderPermissions
	, manageCompany
	)
  
where 
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql as Postgresql 
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ErrorCommand 
import Data.Text as T 
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import Data.Aeson
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)

import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Time
import CCAR.Main.Util 
data CRUD = Create | Read | C_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)

data ManageCompany = ManageCompany {
        nickName :: T.Text
        , crudType :: CRUD 
        , survey :: Company 
    } deriving (Show, Eq)


type CompanyID = T.Text 


insertCompany :: NickName -> Company -> IO Company 
insertCompany aNickName aCompany = do 
	currentTime <- getCurrentTime 
	x <- dbOps $ do 
			insert aCompany {companySignupTime = currentTime}
	return aCompany

deleteCompany :: Company -> IO Company 
deleteCompany aCompanyId = dbOps $ 
	do 
		company <- getBy $ CompanyUniqueID (companyCompanyID aCompanyId)
		case company of 
			Just (Entity k v) -> 
				do 
					delete k 
					return v

updateCompany :: NickName -> Company -> IO Company 
updateCompany aNickName aCompany = do 
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		person <- getBy $ PersonUniqueNickName aNickName 
		case person of 
			Just (Entity p _ ) -> do 
				company <- getBy $  CompanyUniqueID (companyCompanyID aCompany)
				case company of
					Just (Entity k v) -> do 
						Postgresql.replace k $ 
							aCompany {companyUpdatedTime = currentTime
									  , companyUpdatedBy = p }
						return v
	return x 


queryCompany aNickName aCompany = do 
	x <- dbOps $ getBy $ CompanyUniqueID (companyCompanyID aCompany)
	case x of 
		Just (Entity p v) -> return v

insertCompanyPerson :: NickName -> CompanyID -> Bool -> IO ()
insertCompanyPerson aNickName aCompanyId chatMinder = do
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		personId <- getBy $ PersonUniqueNickName aNickName
		cid <- getBy $ CompanyUniqueID aCompanyId 
		case (personId, cid)  of 
			(Just (Entity k1 p1) , Just (Entity k2 p2)) -> do 
					insert $ CompanyUser k2 k1 chatMinder False (Just "en_US")
	return ()


assignSupportForCompany :: NickName -> CompanyID -> Bool -> IO ()
assignSupportForCompany aNickName aCompanyId support = do 
	x <- dbOps $ do 
		personId <- getBy $ PersonUniqueNickName aNickName
		cid <- getBy $ CompanyUniqueID aCompanyId 
		case(personId, cid) of 
				(Just (Entity k1 p1), Just (Entity k2 p2)) -> do 
					pcid <- getBy $ UniqueCompanyUser k2 k1
					case pcid of 
						Just (Entity k3 p3) -> update k3 [CompanyUserSupport =. support]
	return ()

type Locale = T.Text 
updateCompanyPersonLocale :: NickName -> CompanyID -> Locale -> IO () 
updateCompanyPersonLocale aNickName aCompanyId aLocale = do 
	x <- dbOps $ do 
		personId <- getBy $ PersonUniqueNickName aNickName
		cid <- getBy $ CompanyUniqueID aCompanyId 
		case(personId, cid) of 
				(Just (Entity k1 p1), Just (Entity k2 p2)) -> do 
					pcid <- getBy $ UniqueCompanyUser k2 k1
					case pcid of 
						Just (Entity k3 p3) -> update k3 [CompanyUserLocale =. (Just aLocale) ]
	return ()

updateCompanyChatMinder :: NickName -> CompanyID -> Bool -> IO () 
updateCompanyChatMinder aNickName aCompanyId chatMinder = do 
	x <- dbOps $ do 
		personId <- getBy $ PersonUniqueNickName aNickName
		cid <- getBy $ CompanyUniqueID aCompanyId 
		case(personId, cid) of 
				(Just (Entity k1 p1), Just (Entity k2 p2)) -> do 
					pcid <- getBy $ UniqueCompanyUser k2 k1
					case pcid of 
						Just (Entity k3 p3) -> update k3 [CompanyUserChatMinder =. chatMinder ]
	return ()

promoteToChatMinder :: NickName -> CompanyID -> IO ()
promoteToChatMinder aNickName aCompanyId = updateCompanyChatMinder aNickName aCompanyId True 

revokeChatMinderPermissions :: NickName -> CompanyID -> IO() 
revokeChatMinderPermissions aNickName aCompanyId = updateCompanyChatMinder aNickName aCompanyId False

process c@(ManageCompany nickName crudType company) =  do 
	case crudType of 
	        Create -> insertCompany nickName company		            		
	        C_Update -> updateCompany nickName company 
	        Read -> queryCompany nickName company
	        Delete -> deleteCompany company

manageCompany aNickName (Object a) = do 
    case (parse parseManageCompany a) of
        Success r -> do 
        		company <- process r  
        		return (GC.Reply, 
        				serialize company) 
        Error s -> return (GC.Reply, 
                    serialize $ genericErrorCommand $ "Sending message failed " ++ s ++ (show a))


gen (ManageCompany nickName crudType company) = object ["crudType" .= crudType
                    , "company" .= company
                    , "commandType" .= ("ManageCompany" :: T.Text)
                    , "nickName" .= nickName]

parseManageCompany v = ManageCompany <$>
                    v .: "nickName" <*> 
                    v .: "crudType" <*>
                    v .: "company"


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON ManageCompany where
    toJSON  = gen

instance FromJSON ManageCompany where 
    parseJSON (Object v ) = parseManageCompany v 
    parseJSON _           = Appl.empty


