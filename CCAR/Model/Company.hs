module CCAR.Model.Company
	(createSBR 
	, insertCompanyPerson
	, updateCompanyPersonLocale
	, promoteToChatMinder
	, revokeChatMinderPermissions
	)
  
where 
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql
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


-- A default company 
createSBR :: IO ()
createSBR = do 
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		sbrId <- getBy $ CompanyUniqueID "sbr"
		case sbrId of
			Nothing -> do
				nId <- insert $ Company "Sarva Bio Remed" "sbr" currentTime "sales@sarvabioremed.com"
				nDId <- insert $ CompanyDomain nId "www.sarvabioremed.com" "sbr_logo.jpg" "Fuel oil fumes gone!"
							"Reliable Environmental Solutions at your fingertips"
				nCId <- insert $ CompanyContact nId EnumeratedTypes.Email "satyaganti@sarvabioremed.com"
				return (nId, nDId, nCId)
	return ()

type CompanyID = T.Text 


insertCompanyPerson :: NickName -> CompanyID -> Bool -> IO ()
insertCompanyPerson aNickName aCompanyId chatMinder = do
	currentTime <- getCurrentTime
	x <- dbOps $ do 
		personId <- getBy $ PersonUniqueNickName aNickName
		cid <- getBy $ CompanyUniqueID aCompanyId 
		case (personId, cid)  of 
			(Just (Entity k1 p1) , Just (Entity k2 p2)) -> do 
					insert $ CompanyUser k2 k1 chatMinder  (Just "en_US")
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

