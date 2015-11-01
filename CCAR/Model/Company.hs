{--License: license.txt --}
module CCAR.Model.Company
	( promoteToChatMinder
	, revokeChatMinderPermissions
	, manageCompany
	, parseManageCompany
	, process
	, queryAllCompanies
	, ManageCompany
	, CompanyT
	, assignUserToCompany
	) where 
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql as Postgresql 
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ApplicationError 
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
import System.Log.Logger as Logger

data CRUD = Create | Read | C_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)


data AssignUser = AssignUser {
	auCommandType :: T.Text 
	, auCompanyID :: T.Text 
	, auUserName :: T.Text 
	, isChatMinder :: Bool 
	, isSupport :: Bool 
} deriving (Show, Eq, Read, Data, Generic, Typeable)

data CompanyT = CompanyT {
		companyName :: T.Text
		, companyID :: T.Text
		, companyImage :: T.Text
		, generalMailbox :: T.Text		
	} deriving(Show, Eq)


data QueryCompany = QueryCompany {
	qNickName :: T.Text
	, commandType :: T.Text
	, qCompany :: [CompanyT] 
}

data ManageCompany = ManageCompany {
        nickName :: T.Text
        , crudType :: CRUD 
        , company :: CompanyT 
    } deriving (Show, Eq)


type CompanyID = T.Text 


createQueryCompanyDTO :: NickName -> Company -> CompanyT
createQueryCompanyDTO a co = 
			CompanyT (companyCompanyName co )
					 (companyCompanyID co)
					 (companyCompanyImage co) 
					 (companyGeneralMailbox co)	

-- A type converter. The dto/dao hibernate model 
-- seems to live on!
manageCommandDTO :: NickName -> CRUD -> Maybe Company -> ManageCompany 
manageCommandDTO a c (Just co) = 
	ManageCompany a c $
			CompanyT (companyCompanyName co )
					 (companyCompanyID co)
					 (companyCompanyImage co) 
					 (companyGeneralMailbox co)

manageCommandDTO a c Nothing = 
	ManageCompany a c $
			CompanyT "" "" "" ""

iModuleName :: String
iModuleName = "CCAR.Model.Company"

insertCompany :: NickName -> CompanyT -> IO (Maybe Company)
insertCompany aNickName aCompany = do 
	currentTime <- getCurrentTime 
	Logger.debugM iModuleName $ 
					"Inserting " `mappend` (show aCompany)
	dbOps $ do 
		person <- getBy $ UniqueNickName aNickName
		case person of 
			Just(Entity personId v) -> do  
				insert c
				return $ Just c
				where 
					c = Company 
							(companyName aCompany)
							(companyID aCompany)
							(generalMailbox aCompany)
							(companyImage aCompany)
							personId
							currentTime					
							currentTime



deleteCompany :: CompanyT -> IO (Maybe Company)
deleteCompany aCompanyId = dbOps $ 
	do
		x <- runMaybeT $ do 
			Just (Entity k v) <- lift $ getBy $ UniqueCompanyId (companyID aCompanyId) 
			liftIO $ Logger.debugM iModuleName $ 
					"Deleting " `mappend` (show aCompanyId) 
			lift $ delete k 
			return v 
		return x

selectAllCompanies :: IO [Entity Company]
selectAllCompanies = dbOps $ do 
	liftIO $ Logger.debugM iModuleName "Selecting all companies"
	selectList [] [LimitTo resultsPerPage]
					where resultsPerPage = 100

updateCompany :: NickName -> CompanyT -> IO (Maybe Company)
updateCompany aNickName aCompany@(CompanyT tName tID tImage tGen) = do 
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		runMaybeT $ do 
			Just (Entity p person) <- lift $ getBy $ UniqueNickName aNickName 
			liftIO $ Logger.debugM iModuleName $ 
							"Updating company " `mappend` (show person)
			Just (Entity k v) <- lift $ getBy $ UniqueCompanyId tID 
			res <- return $ v { companyCompanyName = tName
					, companyCompanyImage = tImage
					, companyGeneralMailbox = tGen
					, companyUpdatedTime = currentTime
					, companyUpdatedBy = p } -- XXX: Change it back to p
			lift $ Postgresql.replace k res
			return res
	return x 



queryCompany aNickName aCompany = do 
	x <- dbOps $ getBy $ UniqueCompanyId (companyID aCompany)
	y <- case x of 
		Just (Entity p v) -> return $ Just v
		Nothing -> return Nothing
	return y 



insertCompanyPerson :: NickName -> CompanyID -> Bool -> IO (Either T.Text (Key CompanyUser))
insertCompanyPerson aNickName aCompanyId chatMinder = do
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		personId <- getBy $ UniqueNickName aNickName
		cid <- getBy $ UniqueCompanyId aCompanyId 

		liftIO $ Logger.debugM iModuleName $ "Company " ++ (show cid)
		liftIO $ Logger.debugM iModuleName $ "Person " ++ (show personId)
		case (personId, cid)  of 
			(Just (Entity k1 p1) , Just (Entity k2 p2)) -> do 
					cuid <- getBy $ UniqueCompanyUser k2 k1
					case cuid of 
						Nothing -> do 
								v <- insert $ CompanyUser k2 k1 chatMinder False (Just "en_US")
								return $ Right v
						Just _ -> do 
							liftIO $ Logger.infoM iModuleName $  "Entity exists. Not inserting"
							return $ Left "Entity exists. Not inserting"
			(_, _ ) ->do 
				liftIO $ Logger.errorM iModuleName $ 
						"Error inserting company user " 
						`mappend` (T.unpack aNickName) 
						`mappend` " " 
						`mappend` (T.unpack aCompanyId) 
				return $ Left "Insert/update failed"
	return x


queryCompanyUser n c = do
	x <- dbOps $ do
		personId <- getBy $ UniqueNickName n 
		cId <- getBy $ UniqueCompanyId c 
		x <- case (personId, cId) of 
			(Just (Entity p1 pV), Just (Entity c1 cV)) -> getBy $ UniqueCompanyUser c1 p1
		return x
	return x
assignSupportForCompany :: NickName -> CompanyID -> Bool -> IO ()
assignSupportForCompany aNickName aCompanyId support = do 
	x <- dbOps $ do 
		personId <- getBy $ UniqueNickName aNickName
		cid <- getBy $ UniqueCompanyId aCompanyId 
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
		personId <- getBy $ UniqueNickName aNickName
		cid <- getBy $ UniqueCompanyId aCompanyId 
		case(personId, cid) of 
				(Just (Entity k1 p1), Just (Entity k2 p2)) -> do 
					pcid <- getBy $ UniqueCompanyUser k2 k1
					case pcid of 
						Just (Entity k3 p3) -> update k3 [CompanyUserLocale =. (Just aLocale) ]
	return ()

updateCompanyChatMinder :: NickName -> CompanyID -> Bool -> IO () 
updateCompanyChatMinder aNickName aCompanyId chatMinder = do 
	x <- dbOps $ do 
		personId <- getBy $ UniqueNickName aNickName
		cid <- getBy $ UniqueCompanyId aCompanyId 
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

manageCompany aNickName o@(Object a) = do 
	case (parse parseJSON o :: Result ManageCompany) of
	    Success r@(ManageCompany a cType company) -> do
	    		company <- process r  
	    		return (GC.Reply, 
	    				serialize $ manageCommandDTO aNickName cType company)
	    Error s -> do 
			  return (GC.Reply, 
				    	serialize $ appError $ 
				    		"Sending message failed " ++ s)

queryAllCompanies aNickName o@(Object a) = do 
		case (parse parseJSON o :: Result QueryCompany) of 
			Success r -> do 
					companiesD <- selectAllCompanies
					companiesT <- mapM (\x@(Entity k v) -> 
											return $ createQueryCompanyDTO aNickName v) 
										companiesD
					return (GC.Reply
							, serialize $ QueryCompany aNickName "SelectAllCompanies" companiesT)
			Error s -> 	return (GC.Reply, 
				    	serialize $ appError $ 
				    		"Query all companies failed " ++ s)


assignUserToCompany aNickName aValue = do 
	case (parse parseJSON aValue :: Result AssignUser) of 
		Success a@(AssignUser cType cID user chatMinder support) -> do 
			x <- insertCompanyPerson user cID chatMinder 
			case x of 
				Right y -> return (GC.Reply, serialize a)
				Left z -> return (GC.Reply, serialize $ 
										a{auCompanyID = z, auUserName = z})
		Error errorMsg -> return (GC.Reply, 
					serialize $ appError $ 
						"Error assignUserToCompany " ++ errorMsg)

gen (ManageCompany nickName crudType company) = object ["crudType" .= crudType
                    , "company" .= company
                    , "commandType" .= ("ManageCompany" :: T.Text)
                    , "nickName" .= nickName]

parseQueryCompany v = do 
				QueryCompany <$> 
					v .: "nickName" <*>
					v .: "commandType" <*>
					pure []
					

genQueryCompany (QueryCompany n cType com) = 
				object 
				["nickName" .= n 
				, "crudType" .= cType 
				, "commandType" .= ("SelectAllCompanies" :: T.Text)
				, "company" .= com
				]
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



instance ToJSON AssignUser where 
	toJSON (AssignUser cType cID userName isChatMinder isSupport) = 
			object [
				"commandType" .= cType
				, "companyID" .= cID
				, "userName " .= userName 
				, "isChatMinder" .= isChatMinder
				, "isSupport" .= isSupport
			]

instance FromJSON AssignUser where 
	parseJSON (Object v) = AssignUser <$> 
					v .: "commandType"  <*>
					v .: "companyID" <*> 
					v .: "userName" <*>
					v .: "isChatMinder" <*>
					v .: "isSupport"
	parseJSON _ 		= Appl.empty




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

instance ToJSON QueryCompany where
	toJSON = genQueryCompany 

instance FromJSON QueryCompany where
	parseJSON (Object v) = parseQueryCompany v 
	parseJSON _ 		 = Appl.empty


