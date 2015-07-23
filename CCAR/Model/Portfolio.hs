module CCAR.Model.Portfolio (
	queryPortfolioSymbolTypes
	, queryPortfolioSymbolSides
	, process
	, PortfolioT(..)
	, Portfolio(..)
	, testCreatePortfolioT
	, testCreatePortfolioTJSON
	, testParseCreatePortfolioTJSON
	, uuidFromString
	) where 
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core

import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import Control.Exception
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
import GHC.IO.Exception

import Data.Data
import Data.Monoid (mappend)
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
import qualified CCAR.Main.EnumeratedTypes as EnTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util as Util
import CCAR.Command.ErrorCommand
import Database.Persist.Postgresql as Postgresql 
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))
import System.Log.Logger as Logger


--Helper functions 

iModuleName = "CCAR.Model.Portfolio"
uuidAsString = UUID.toString 
uuidFromString aString = do
	case (x aString) of 
		Just y -> y  
	where 
		x = UUID.fromString

-- Data types
data CRUD = Create | Read | P_Update | Delete 
	deriving(Show, Read, Eq, Data, Generic, Typeable)
	

data PortfolioCommands = PortfolioSymbolTypesQuery | PortfolioSymbolSidesQuery
		deriving (Show, Read, Data, Typeable, Generic)

data PortfolioSymbolTypeQuery = PortfolioSymbolTypeQuery {
		nickName :: T.Text
		, commandType :: T.Text
		, symbolTypes :: [EnTypes.PortfolioSymbolType]
} deriving(Show, Read, Data, Typeable, Generic)

{-- 
 Name mangling: https://mail.haskell.org/pipermail/haskell-cafe/2010-December/087862.html
--}
data PortfolioSymbolSideQuery = PortfolioSymbolSideQuery {
	pssNickName :: T.Text
	, pssCommandType :: T.Text
	, symbolSides :: [EnTypes.PortfolioSymbolSide]
} deriving(Show, Read, Data, Typeable, Generic)




-- DB queries

queryPortfolioSymbolTypes :: T.Text -> Value -> IO(GC.DestinationType, T.Text)
queryPortfolioSymbolTypes n (Object a) = 
			return(GC.Reply, 
					Util.serialize $ PortfolioSymbolTypeQuery n 
					(T.pack $ show PortfolioSymbolTypesQuery)
					EnTypes.getPortfolioSymbolTypes)
queryPortfolioSymbolSides :: T.Text -> Value -> IO (GC.DestinationType, T.Text)
queryPortfolioSymbolSides n (Object a) = 
		return (GC.Reply, 
				Util.serialize $ 
					PortfolioSymbolSideQuery n 
						(T.pack $ show PortfolioSymbolSidesQuery)
						EnTypes.getPortfolioSymbolSides)

type INickName = T.Text
type PortfolioUUID = T.Text 
type CompanyID = T.Text

data PortfolioT = PortfolioT {
	crudType :: CRUD
	, portfolioId :: PortfolioUUID
	, companyId :: CompanyID
	, userId :: INickName
	, summary :: T.Text 
	, createdBy :: INickName
	, updatedBy :: INickName
} deriving(Show, Read, Eq, Data, Generic, Typeable)

data PortfolioQuery = PortfolioQuery {
	qCompanyId :: CompanyID 
	, qUserId :: INickName
	, resultSet :: [Either T.Text PortfolioT]
} deriving (Show, Read, Eq, Data, Generic, Typeable)

queryPortfolios :: PortfolioQuery -> IO (Either T.Text PortfolioQuery)
queryPortfolios query = do 
	dbOps $ do 
		company <- getBy $ CompanyUniqueID $ qCompanyId query 
		user <- getBy $ PersonUniqueNickName $ qUserId query
		case (company, user) of 
			(Just (Entity cId _) , Just (Entity uId _)) -> do 
				companyUserId <- getBy $ UniqueCompanyUser cId uId
				case companyUserId of 
					Just (Entity cuId value) -> do 
						portfolios <- selectList [PortfolioCompanyUserId ==. cuId] []
						portfolioTs <- liftIO $ mapM (\(Entity k p) -> daoToDto Read k) portfolios
						return $ Right $ query {resultSet = portfolioTs}
					Nothing -> return $ Left $ "Query portfolios failed"

instance ToJSON PortfolioQuery
instance FromJSON PortfolioQuery

instance ToJSON PortfolioT where
	toJSON p1@(PortfolioT c p c1 u s cr up) =
		object [
			"crudType" .= c 
			, "portfolioId" .= p 
			, "companyId" .= c1 
			, "userId" .= u 
			, "summary" .= s 
			, "createdBy" .= cr 
			, "updatedBy" .= up
		]

instance FromJSON PortfolioT where
	parseJSON (Object a) = PortfolioT <$>
				a .: "crudType" <*>
				a .: "portfolioId" <*>
				a .: "companyId" <*> 
				a .: "userId" <*>
				a .: "summary" <*>
				a .: "createdBy" <*>
				a .: "updatedBy" 
	parseJSON _ 	= Appl.empty  

sanityCheck :: Key Portfolio -> PortfolioT -> Either T.Text Bool
sanityCheck = undefined

dtoToDao :: PortfolioT -> IO (Either T.Text Portfolio)
dtoToDao pT@(PortfolioT cType 
			porId comId userId summary createdBy updatedBy) = do 
	currentTime <- getCurrentTime 
	dbOps $ do 
			com <- getBy $ CompanyUniqueID comId 
			porKV <- getBy $ UniquePortfolio porId 
			case porKV of 
				Just (Entity pKey pValue) -> return $ Right pValue
				Nothing -> return $ Left $  "Portfolio id " `mappend` porId `mappend` " not found"


-- Optimisation note? or have we given up on database caches and let the db do its thing --
daoToDto :: CRUD -> PortfolioId -> IO (Either T.Text PortfolioT)
daoToDto crType pid = do 
	currentTime <- getCurrentTime 
	dbOps $ do
		porKV <- get pid 
		case porKV of 
			Just v -> do 
				comUser <- get (portfolioCompanyUserId v)
				case comUser of 
					Just coUser1 -> do 				
						company <- get (companyUserCompanyId coUser1)
						cUser <- get (companyUserUserId coUser1) 
						createdBy <- get (portfolioCreatedBy v) 
						updatedBy <- get (portfolioUpdatedBy v)
						case (comUser, company, cUser, createdBy, updatedBy) of
							(Just coUser, Just com, Just cu, Just createdByObj, Just updby) -> 
								return $ Right $ PortfolioT crType 
											(portfolioUuid v) 
											(companyCompanyID com)
											(personNickName cu)
											(portfolioSummary v)
											(personNickName createdByObj) 
											(personNickName updby)
							_-> return $ Left $ T.pack "Unable to create portfolio transfer object.ouch!"

			Nothing -> return $ Left $ T.pack $ 
								"Unable to query db " `mappend` (show pid) 

manage :: Value -> IO (GC.DestinationType, T.Text)
manage aValue@(Object a) = 
	case (fromJSON aValue) of
		Success r -> do 
			res <- process r  
			case res of
				Right k -> do 
					res1 <- daoToDto  (crudType r) k 
					case res1 of 
						Right pT -> 
							case (sanityCheck k pT) of 						
								Right _ -> return (GC.Reply, serialize res)
								Left _ -> do 
									liftIO $ Logger.errorM iModuleName $ 
										("Input uuids and db uuids dont match "  :: String)
										`mappend` (show res)
									return (GC.Reply, serialize $ genericErrorCommand $
												"Input output uuids dont match ")
				Left f -> do 
					liftIO $ Logger.errorM iModuleName $ 
							"Error processing manage portfolio " `mappend` (show aValue)
					return (GC.Reply, serialize $ genericErrorCommand $ 
								"Error processing manage portfolio " ++ (T.unpack f))


process :: PortfolioT -> IO (Either T.Text (Key Portfolio))
process pT = case (crudType pT) of 
	Create -> insertPortfolio pT 
	Read -> readPortfolio pT -- single record
	P_Update -> updatePortfolio pT 
	Delete -> deletePortfolio pT 		





insertPortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio) )
insertPortfolio p@(PortfolioT cType 
				_ 
				companyId 
				userId 
				summary 
				createdBy 
				_)= do 
	uuid <- nextUUID 
	case uuid of 
		Just u -> do 
			currentTime <- getCurrentTime
			dbOps $ do 
				c <- getBy $ CompanyUniqueID companyId 
				nPerson <- getBy $ PersonUniqueNickName userId 
				crBy <- getBy $ PersonUniqueNickName createdBy
				case (c, nPerson, crBy) of 
					(Just (Entity cKey _)
						, Just (Entity nKey _)
						, Just (Entity crKey _) )-> do
						cuser <- getBy $ UniqueCompanyUser cKey nKey
						case cuser of 
							Just (Entity cuKey cuValue) -> do 
								porId <- insert $ Portfolio cuKey 
											(T.pack $ uuidAsString u)
											summary 
											crKey
											currentTime
											nKey 
											currentTime
								return $ Right porId
	 



updatePortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
updatePortfolio p@(PortfolioT cType 
				   portId 
				   companyId 
				   userId 
				   summary 
				   createdBy
				   updatedBy) = do 
	currentTime <- getCurrentTime 
	dbOps $ do 
		portfolio <- getBy $ UniquePortfolio portId
		case portfolio of 
			Nothing -> return $ Left $ T.pack $ "Portfolio not found " `mappend` (show p)
			Just (Entity portfolioId portfolio) -> do 
					update portfolioId [PortfolioSummary =. summary, PortfolioUpdatedOn =. currentTime]
					return $ Right portfolioId

readPortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
readPortfolio p@(PortfolioT cType portId 
		companyId 
		userId 
		summary 
		createdBy 
		updateBy) = do
		Logger.debugM iModuleName $ "Reading portfolio " ++ (show p)	
		dbOps $ do 
			portfolio <- getBy $ UniquePortfolio portId 
			case portfolio of 
				Nothing -> return $ Left $ T.pack $ "Portfolio not found " `mappend` (show p)
				Just (Entity pId port) -> return $ Right  pId 

deletePortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
deletePortfolio p@(PortfolioT cType
	portId 
	companyId 
	userId 
	summary 
	createdBy
	updatedBy) = do 
	Logger.debugM iModuleName $ "Deleting portfolio " ++ (show p)
	dbOps $ do 
		portfolio <- getBy $ UniquePortfolio portId 
		case portfolio of
			Nothing -> return $ Left $ T.pack $ "Portfolio not found " `mappend` (show p)  
			Just(Entity pid port) -> do
				liftIO $ Logger.debugM iModuleName $ "Deleting pid " `mappend` (show p)  
				delete pid 
				return $ Right pid



-- Test scripts
testCreatePortfolioT :: IO PortfolioT 
testCreatePortfolioT = do 
	uuid <- nextUUID 
	case uuid of 
		Just u -> return $ PortfolioT Create (T.pack $ uuidAsString u) "test_company" "test" "Test script" "test" "test"

testCreatePortfolioTJSON :: IO T.Text 
testCreatePortfolioTJSON = do 
		x <- testCreatePortfolioT
		return $ serialize x


testParseCreatePortfolioTJSON = do
	x <- testCreatePortfolioTJSON
	putStrLn $ show x
	return $ J.eitherDecode $ E.encodeUtf8 $ L.fromStrict x


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON PortfolioCommands 
instance FromJSON PortfolioCommands
instance ToJSON PortfolioSymbolTypeQuery
instance FromJSON PortfolioSymbolTypeQuery 
instance ToJSON PortfolioSymbolSideQuery 
instance FromJSON PortfolioSymbolSideQuery
