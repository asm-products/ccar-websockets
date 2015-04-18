{--License: license.txt --}
module CCAR.Model.Project
(
	manageProject
)

where
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import qualified  Data.Map as IMap
import Control.Exception
import Control.Monad
import Control.Monad.Logger(runStderrLoggingT)
import Network.WebSockets.Connection as WSConn
import Data.Text as T
import Data.Text.Lazy as L 
import Database.Persist.Postgresql as DB
import Data.Aeson.Encode as En
import Data.Text.Lazy.Encoding as E
import Data.Aeson as J
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import GHC.Generics
import Data.Data
import Data.Typeable 
import System.IO
import Data.Time
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util 
import CCAR.Command.ErrorCommand
import Database.Persist.Postgresql as Postgresql 


{-- 
 Create a project.
 Assign a set of images to a project
 assign a set of documents to project
--}
data CRUD = Create | Read | P_Update | Delete 
				deriving(Show, Eq, Data, Generic, Typeable)

instance ToJSON CRUD 
instance FromJSON CRUD

data ProjectT = ProjectT {
		crudType :: CRUD
		, identification :: T.Text
		, companyUniqueId :: T.Text 
		, summary :: T.Text 
		, details :: T.Text
		, startDate :: UTCTime
		, endDate :: UTCTime
		, uploadedBy :: T.Text
		, uploadTime :: UTCTime 
		, preparedBy :: T.Text	
	} deriving (Show, Eq, Data, Generic, Typeable)


insertProject t@(ProjectT c i cid s de sd ed up upT pr) = do 
	upT <- getCurrentTime 
	dbOps $ do 
		uploader <- getBy $ PersonUniqueNickName up 
		company <- getBy $ CompanyUniqueID cid 
		case (uploader, company) of 
			(Just (Entity personId personV)
				, Just(Entity companyId cValue)) -> do 
				insert $ Project i companyId s de sd ed personId upT pr
				return $ Right t 


readProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
	company <- getBy $ CompanyUniqueID cid  
	case company of 
		Just (Entity k v) -> do 
			project <- getBy $ UniqueProject i k
			case project of 
				-- Assert that k and k2 are the same
				Just (Entity k (Project i k2 s de sd ed u upT pr)) -> do 
						person <- get u 
						case person of 
							Just (Person f l nn pass lo lastLogin) -> 
								return $ Right $ ProjectT Read i (companyCompanyID v ) s de sd ed nn upT pr
							Nothing -> return $ Left ("reading project failed" :: T.Text)

updateProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
	company <- getBy $ CompanyUniqueID cid
	case company of 
		Just (Entity k v) -> do 
						project <- getBy $ UniqueProject i k
						case project of 
							Just (Entity k2 prValue) -> do 	
								res <- return  $ prValue {projectSummary = s 
											, projectDetails = de 
											, projectStartDate = sd
											, projectEndDate = ed 
											, projectPreparedBy = pr}
								Postgresql.replace k2 res 
								return $ Right t

deleteProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
		company <- getBy $ CompanyUniqueID cid 
		case company of 
			Just (Entity k v) -> do 
					project <- getBy $ UniqueProject i k 
					case project of 
						Just (Entity k2 prValue) -> do 
								Postgresql.delete k2 
								return $ Right t 
processP t@(ProjectT c i cid s de sd ed up upT pr) = return $ 
	case c of 
		Create -> insertProject t 
		Read -> readProject t 
		P_Update -> updateProject t 
		Delete -> deleteProject t

manageProject aNickName (Object a) = do
	case (parse parseJSON (Object a)) of 
		Success r -> do 
				case project of 
					Right x -> do 
						reply <- x 
						return (GC.Reply, 
								serialize reply)
					Left y -> return (GC.Reply, 
							serialize $ genericErrorCommand $ "Project processing failed ")
				where 
					project = processP r
		Error s -> 
			  return (GC.Reply, 
		    	serialize $ genericErrorCommand $ 
		    		"Sending message failed " ++ s)



instance FromJSON ProjectT where 
    parseJSON (Object a) = ProjectT <$> 
								(a .: "crudType") <*>
								(a .: "projectId") <*>
								(a .: "uniqueCompanyID") <*>
								(a .: "summary") <*>
								(a .: "details") <*>
								(a .: "startDate") <*>
								(a .: "endDate") <*>
								(a .: "uploadedBy") <*> 
								(a .: "uploadeTime") <*>
								(a .: "preparedBy")

    parseJSON _          = Appl.empty



instance ToJSON ProjectT where
	toJSON p@(ProjectT cType pid cid 
			summary 
			details
			startDate
			endDate
			uploadedBy 
			uploadTime 
			preparedBy) = 
			object [
				"commandType" .= ("ManageProject" :: T.Text)
				, "crudType" .= cType
				, "projectId" .= pid 
				, "uniqueCompanyID" .= cid 
				, "summary" .= summary
				, "details" .= details 
				, "startDate" .= startDate
				, "endDate" .= endDate 
				, "uploadedBy" .= uploadedBy 
				, "uploadTime" .= uploadTime
				, "preparedBy" .= preparedBy
			] 
