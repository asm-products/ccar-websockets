{--License: license.txt --}
module CCAR.Model.Project
(
	manageProject
	, processP
	, queryActiveProjects
	, parseDate
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
import Data.HashMap.Lazy as LH (HashMap, lookup)
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import GHC.Generics
import Data.Data
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
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
		, startDate :: Maybe UTCTime
		, endDate :: Maybe UTCTime
		, uploadedBy :: T.Text
		, uploadTime :: Maybe UTCTime 
		, preparedBy :: T.Text	
	} deriving (Show, Eq, Data, Generic, Typeable)


data QueryProject = QueryProject {
	qNickName :: T.Text
	, commandType :: T.Text
	, companyId :: T.Text
	, qCompany :: [Project] 
}


insertProject :: ProjectT -> IO (Either T.Text ProjectT)
insertProject t@(ProjectT c i cid s de sd ed up upT pr) = do 
	uuid <- nextUUID
	case uuid of 
		Just uuidJ -> do
			dbOps $ do 
				uploader <- getBy $ PersonUniqueNickName up 
				company <- getBy $ CompanyUniqueID cid
				case (uploader, company) of 
					(Just (Entity personId personV)
						, Just(Entity companyId cValue)) -> do 
						insert $ do 
								Project companyUniId 
									companyId s de sd ed personId upT pr
						return $ Right t {identification = companyUniId}
						where
							companyUniId = T.pack $ UUID.toString uuidJ 
		Nothing -> return $ Left ("UUID generated too quickly?" :: T.Text)


readProject :: ProjectT -> IO (Either T.Text ProjectT)
readProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
	company <- getBy $ CompanyUniqueID cid	
	case company of 
		Just (Entity k v) -> do 
			project <- getBy $ UniqueProject i
			case project of 
				-- Assert that k and k2 are the same
				Just (Entity k (Project uuid k2 s de sd ed u upT pr)) -> do 
						person <- get u 
						case person of 
							Just (Person f l nn pass lo lastLogin) -> 
								return $ Right $ ProjectT Read 
										i (companyCompanyID v ) s de sd ed nn upT pr
							Nothing -> return $ Left ("reading project failed" :: T.Text)

updateProject :: ProjectT -> IO (Either T.Text ProjectT)
updateProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
	company <- getBy $ CompanyUniqueID cid
	case company of 
		Just (Entity k v) -> do 
						project <- getBy $ UniqueProject i
						case project of 
							Just (Entity k2 prValue) -> do 	
								res <- return  $ prValue {projectSummary = s 
											, projectDetails = de 
											, projectStartDate = sd
											, projectEndDate = ed 
											, projectPreparedBy = pr}
								Postgresql.replace k2 res 
								return $ Right t

deleteProject :: ProjectT -> IO (Either T.Text ProjectT)
deleteProject t@(ProjectT c i cid s de sd ed up upT pr) = dbOps $ do
		company <- getBy $ CompanyUniqueID cid 
		case company of 
			Just (Entity k v) -> do 
					project <- getBy $ UniqueProject i 
					case project of 
						Just (Entity k2 prValue) -> do 
								Postgresql.delete k2 
								return $ Right t 
processP :: Monad m => ProjectT -> m (IO (Either T.Text ProjectT))
processP t@(ProjectT c i cid s de sd ed up upT pr) = return $ 
	case c of 
		Create -> insertProject t 
		Read -> readProject t 
		P_Update -> updateProject t 
		Delete -> deleteProject t


manageProject :: t -> Value -> IO (GC.DestinationType, T.Text)
manageProject aNickName (Object a) = do
	case (parse parseJSON (Object a)) of 
		Success r -> do 
				case project of 
					Right x -> do 
							mProject <- x 
							return (GC.Reply, 
								serialize mProject)
					Left y -> return (GC.Reply, 
							serialize $ genericErrorCommand $ "Project processing failed " 
									++ y)
				where 
					project = processP r
		Error s -> 
			  return (GC.Reply, 
		    	serialize $ genericErrorCommand $ 
		    		"Manage project failed " ++ s)


queryActiveProjects aNickName (Object a) = do 
	case (parse parseQueryProject a) of
		Success r@(QueryProject n cType cid _) -> do
				ap <- selectActiveProjects cid
				resAP <- mapM (\x@(Entity k v) -> return v) ap 
				return (GC.Reply, 
						serialize $ QueryProject n cType cid resAP)
		Error s -> return (GC.Reply, 
						serialize $ genericErrorCommand $ 
							"Query active projects failed " ++ s)

type CompanyUniqueID = T.Text

selectActiveProjects  :: CompanyUniqueID-> IO [Entity Project]
selectActiveProjects x = dbOps $ do 
				company <- getBy $ CompanyUniqueID x
				case company of 
					Just (Entity k v) -> selectList [ProjectCompanyId ==. k ][]


parseDate (Just aDate) = undefined

instance FromJSON ProjectT where 
    parseJSON (Object a) = ProjectT <$> 
								(a .: "crudType") <*>
								(a .: "projectId") <*>
								(a .: "uniqueCompanyID") <*>
								(a .: "summary") <*>
								(a .: "details") <*>
								--parseDate (LH.lookup "startDate" a) <*>
								pure Nothing <*>
								-- parseDate (LH.lookup "endDate" a ) <*>
								pure Nothing <*> 
								(a .: "uploadedBy") <*> 
								-- parseDate (LH.lookup "uploadTime" a) <*>
								pure Nothing <*>
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


parseQueryProject v = do 
				QueryProject <$> 
					v .: "nickName" <*>
					v .: "commandType" <*>
					v .: "companyId" <*>
					pure []
					

genQueryProject (QueryProject n cType cId projects) = 
				object 
				["nickName" .= n 
				, "crudType" .= cType 
				, "commandType" .= ("SelectActiveProjects" :: T.Text)
				, "companyId" .= cId
				, "projects" .= projects
				]

instance ToJSON QueryProject where 
	toJSON = genQueryProject 

instance FromJSON QueryProject where
	parseJSON (Object a) = parseQueryProject a 
	parseJSON _			= Appl.empty