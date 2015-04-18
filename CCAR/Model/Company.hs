{--License: license.txt --}
module CCAR.Model.Company
	( promoteToChatMinder
	, revokeChatMinderPermissions
	, manageCompany
	, parseManageCompany
	, process
	, ManageCompany(..)
	) where 
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
import Data.Monoid
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Time
import CCAR.Main.Util 
data CRUD = Create | Read | C_Update | Delete | SelectAllCompanies
    deriving(Show, Eq, Read, Data, Generic, Typeable)

data CompanyT = CompanyT {
		companyName :: T.Text
		, companyID :: T.Text
		, companyImage :: T.Text
		, generalMailbox :: T.Text		
	} deriving(Show, Eq)


data ManageCompany = ManageCompany {
        nickName :: T.Text
        , crudType :: CRUD 
        , company :: CompanyT 
    } deriving (Show, Eq)


type CompanyID = T.Text 



-- A type converter. The dto/dao hibernate model 
-- seems to live on!
manageCommandDTO :: NickName -> CRUD -> Company -> ManageCompany 
manageCommandDTO a c co = 
	ManageCompany a c $
			CompanyT (companyCompanyName co )
					 (companyCompanyID co)
					 (companyCompanyImage co) 
					 (companyGeneralMailbox co)

insertCompany :: NickName -> CompanyT -> IO Company 
insertCompany aNickName aCompany = do 
	currentTime <- getCurrentTime 
	putStrLn "Inside insert company"
	dbOps $ do 
		person <- getBy $ PersonUniqueNickName aNickName
		case person of 
			Just(Entity personId v) -> do  
				insert c
				return c
				where 
					c = Company 
							(companyName aCompany)
							(companyID aCompany)
							(generalMailbox aCompany)
							(companyImage aCompany)
							personId
							currentTime					
							currentTime



deleteCompany :: CompanyT -> IO Company 
deleteCompany aCompanyId = dbOps $ 
	do 
		company <- getBy $ CompanyUniqueID (companyID aCompanyId)
		case company of 
			Just (Entity k v) -> 
				do 
					delete k 
					return v

selectAllCompanies = dbOps $ do
					companies <- selectList [] [LimitTo resultsPerPage]
					mapM (\(Entity k v) -> v) companies
					where resultsPerPage = 10

updateCompany :: NickName -> CompanyT -> IO Company 
updateCompany aNickName aCompany@(CompanyT tName tID tImage tGen) = do 
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		person <- getBy $ PersonUniqueNickName aNickName 
		case person of 
			Just (Entity p _ ) -> do 
				company <- getBy $  CompanyUniqueID (companyID aCompany)
				case company of
					Just (Entity k v) -> do 
							res <- return v { companyCompanyName = tName
									, companyCompanyImage = tImage
									, companyGeneralMailbox = tGen
									, companyUpdatedTime = currentTime
									, companyUpdatedBy = p } -- XXX: Change it back to p
							Postgresql.replace k res
							return res
	return x 


queryCompany aNickName aCompany = do 
	x <- dbOps $ getBy $ CompanyUniqueID (companyID aCompany)
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
	    Success r@(ManageCompany a cType company) -> do
	    		putStrLn "manage success" 
	    		company <- process r  
	    		return (GC.Reply, 
	    				serialize $ manageCommandDTO aNickName cType company)
	    Error s -> do 
			  return (GC.Reply, 
				    	serialize $ genericErrorCommand $ 
				    		"Sending message failed " ++ s)

queryAllCompanies aNickName (Object a) = do 
		case (parse parseManageCompany a) of 
			Success r -> do 
					companies <- selectAllCompanies 
					companiesT <- mapM (\x@(Entity k v) -> 
										return manageCommandDTO aNickName 
										SelectAllCompanies v) companies
					return (GC.Reply
							, serialize $ companiesT)


gen (ManageCompany nickName crudType company) = object ["crudType" .= crudType
                    , "company" .= company
                    , "commandType" .= ("ManageCompany" :: T.Text)
                    , "nickName" .= nickName]

parseManageCompany v = do
					nickName <- v .: "nickName"
					crudType <- v .: "crudType"
					company  <- v .: "company"
					return $ ManageCompany nickName crudType company


parseCompany v = do 
                companyName <- v .: "companyName"
                companyID <- v .: "companyID"
                companyImage <- v .: "companyImage"
                generalMailbox <- v .: "generalMailbox"
                return $ CompanyT companyName companyID companyImage generalMailbox

genCompany (CompanyT a b c d) = object [
			"companyName" .= a 
			, "companyID" .= b
			, "companyImage" .= c 
			, "generalMailbox" .= d 
	]

instance FromJSON CompanyT where 
    parseJSON (Object v) = parseCompany v
    parseJSON _          = Appl.empty



instance ToJSON CompanyT where
	toJSON = genCompany

instance Show Company where
	show c@(Company cName cID gM _ updatedBy s u ) = 
		(show cName)
		++ ":" 
		++ show cID 
		++ ":"
		++ (show gM) 
		++ ": (image data) " 
		++ ": " ++ (show updatedBy)
		++ ":" ++ (show s)
		++ ":" ++ (show u)


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON ManageCompany where
    toJSON  = gen

instance FromJSON ManageCompany where 
    parseJSON (Object v ) = parseManageCompany v 
    parseJSON _           = Appl.empty


