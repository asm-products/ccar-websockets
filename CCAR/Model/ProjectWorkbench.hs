module CCAR.Model.ProjectWorkbench(
	querySupportedScripts
	, queryActiveWorkbenches
	, manageWorkbench
)where 
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
import Data.Monoid (mappend)
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util as Util
import CCAR.Command.ErrorCommand
import Database.Persist.Postgresql as Postgresql 

data CRUD = Create | Read | WrkBench_Update | Delete 
	deriving(Show, Read, Eq, Data, Generic, Typeable)
	
instance ToJSON CRUD 
instance FromJSON CRUD 


data QuerySupportedScript = QuerySupportedScript {
		nickName :: T.Text
		, commandType :: T.Text
		, scriptTypes :: [EnumeratedTypes.SupportedScript]	
	} deriving (Show, Read, Data, Typeable, Generic)

{-- All of the database entities have 2 faces: the one that 
-- yesod/db layer needs and the other that interfaces with the
-- external interfaces. 
-- Unique project id is the uuid that uniquely identifies the
-- project workbench. Trying to relate objects by their unique 
-- keys is not convenient in the presence of nesting. For example,
-- company -> project-> workbench. --}

data ProjectWorkbenchT = ProjectWorkbenchT {
	crudType :: CRUD
	, workbenchId :: T.Text
	, uniqueProjectId :: T.Text
	, scriptType :: EnumeratedTypes.SupportedScript
	, scriptData :: T.Text
	, numberOfCores :: Int 
	, scriptDataPath :: Maybe T.Text 
	, jobStartDate :: Maybe UTCTime 
	, jobEndDate :: Maybe UTCTime
} deriving(Show, Read, Eq, Data, Generic, Typeable)


dto :: CRUD -> T.Text -> ProjectWorkbench -> ProjectWorkbenchT 
dto c pid p@(ProjectWorkbench  _ w s sdata cores sdPath jobStartDate jobEndDate) = ProjectWorkbenchT 
		c w pid s (sdata) cores sdPath (jobStartDate) (jobEndDate)


process :: ProjectWorkbenchT -> IO (Either T.Text ProjectWorkbench)
process r@(ProjectWorkbenchT cType wId uniqueProjedtId 
				scriptType scriptData 
				numberOfCores
				scriptDataPath
				jobStartDate
				jobEndData 
				) = case cType of 
					Create 			-> insertWorkbench r 
					Read 			-> readWorkbench r
					WrkBench_Update -> updateWorkbench r 
					Delete			-> deleteWorkbench r

deleteWorkbench :: ProjectWorkbenchT -> IO (Either T.Text ProjectWorkbench)
deleteWorkbench w@(ProjectWorkbenchT cType wId uniqueProjedtId 
				scriptType scriptData 
				numberOfCores
				scriptDataPath
				jobStartDate
				jobEndDate) = dbOps $ do 
	workbench <- getBy $ UniqueWorkbench wId 
	case workbench of 
		Just (Entity k v) -> do 
			Postgresql.delete k 
			liftIO $ return $ Right v 
		Nothing -> liftIO $ return $ Left $ "Workbench not found to delete " `mappend` wId


updateWorkbench :: ProjectWorkbenchT -> IO (Either T.Text ProjectWorkbench)
updateWorkbench w@(ProjectWorkbenchT cType wId uniqueProjedtId 
				scriptType scriptData 
				numberOfCores
				scriptDataPath
				jobStartDate
				jobEndDate) = dbOps $ do 
	workbench <- getBy $ UniqueWorkbench wId 
	case workbench of 
		Just (Entity k v) -> do 
			Postgresql.replace k rep 
			liftIO $ return $ Right v 
			where rep = (ProjectWorkbench (projectWorkbenchProject v) wId scriptType
						scriptData
						numberOfCores
						scriptDataPath
						jobStartDate
						jobEndDate)
		Nothing -> liftIO $ return $ Left $ "Workbench not found " `mappend` wId
	
readWorkbench :: ProjectWorkbenchT -> IO (Either T.Text ProjectWorkbench)
readWorkbench w@(ProjectWorkbenchT cType wId uniqueProjedtId 
				scriptType scriptData 
				numberOfCores
				scriptDataPath
				jobStartDate
				jobEndDate) = dbOps $ do 
	workbench <- getBy $ UniqueWorkbench wId 
	case workbench of 
		Just (Entity kWork w) -> return $ Right  w 
		Nothing -> return $ Left $ "Reading workbench " `mappend` wId 



insertWorkbench :: ProjectWorkbenchT -> IO (Either T.Text ProjectWorkbench)
insertWorkbench w@(ProjectWorkbenchT cType wId uniqueProjedtId 
				scriptType scriptData 
				numberOfCores
				scriptDataPath
				jobStartDate
				jobEndDate 
				) = do 
		uuidM <- nextUUID
		case uuidM of 
			Just uuid -> do 
				dbOps $ do 
					project <- getBy $ UniqueProject uuidAsString 
					case project of 
						Just (Entity prKey project) -> do 
							wid <- insert $ ProjectWorkbench prKey 
											uuidAsString
											scriptType
											scriptData 
											numberOfCores
											scriptDataPath 
											jobStartDate
											jobEndDate
							resP <- get wid
							case resP of 
								Just r -> liftIO $ return $ Right r
								Nothing -> liftIO $ return $ Left $ 
												"Entity not found " `mappend` uuidAsString
						Nothing -> liftIO $ return $ Left ("Project " `mappend` uuidAsString 
									`mappend` " not found" :: T.Text)
						where 
							uuidAsString = T.pack $ UUID.toString uuid 
			Nothing -> return $ Left ("UUID generated too quickly")

data QueryAllWorkbenches = QueryAllWorkbenches {
	qaNickName :: T.Text
	, qaCommandType :: T.Text
	, projectId :: T.Text
	, workbenches :: [ProjectWorkbenchT]		
}deriving (Show, Eq, Data, Generic, Typeable)

instance ToJSON QueryAllWorkbenches where 
	toJSON p@(QueryAllWorkbenches n c pid workbenches) = object [
			"nickName" .= n
			, "commandType" .= c 
			, "projectId" .= pid 
			, "workbenches" .= workbenches
		]

instance FromJSON QueryAllWorkbenches where 
	parseJSON (Object a ) = QueryAllWorkbenches <$> 
								(a .: "nickName") <*>
								(a .: "commandType") <*> 
								(a .: "projectId") <*> 
								(a .: "workbenches")
	parseJSON _			  = Appl.empty 

instance ToJSON QuerySupportedScript
instance FromJSON QuerySupportedScript
instance ToJSON ProjectWorkbenchT 
instance FromJSON ProjectWorkbenchT
-- The project uuid (not the internal database id)
selectActiveWorkbenches aProjectId = dbOps $ do 
		project <- getBy $ UniqueProject aProjectId 
		case project of
			Just (Entity k v) -> selectList [ProjectWorkbenchProject ==. k] [LimitTo 50] 
			-- Nothing -> ??

querySupportedScripts :: T.Text -> Value -> IO(GC.DestinationType, T.Text)
querySupportedScripts n (Object a) = 
			return(GC.Reply, 
					Util.serialize $ QuerySupportedScript n 
					"QuerySupportedScripts"
					EnumeratedTypes.getSupportedScripts)


queryActiveWorkbenches aValue@(Object a) = do 
	case (fromJSON aValue) of
		Success (QueryAllWorkbenches n c pid ws) -> do 
				activeW <- selectActiveWorkbenches pid 
				activeWE <- mapM (\x@(Entity k v) -> 
						return $ dto Read pid v) activeW 
				return (GC.Reply, 
						serialize $ QueryAllWorkbenches n c pid activeWE)
		Error errorMsg -> return (GC.Reply, 
					serialize $ genericErrorCommand $ 
						"Error processing query active workbenches " ++ errorMsg)



manageWorkbench :: T.Text -> Value -> IO (GC.DestinationType, T.Text)
manageWorkbench nickName aValue@(Object a) = do 
	case (fromJSON aValue) of 
		Success r -> do 
				res <- process r 
				case res of
					Right r -> return (GC.Reply, serialize r) 
					Left f -> return (GC.Reply, serialize $ genericErrorCommand $ 
								"Error processing manageWorkbench " ++ (T.unpack f))

		Error errorMsg -> return (GC.Reply, 
					serialize $ genericErrorCommand $ 
						"Error in manageworkbench " ++ errorMsg)