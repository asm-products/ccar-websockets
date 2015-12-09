module CCAR.Model.Portfolio (
	queryPortfolioSymbolTypes
	, queryPortfolioSymbolSides
	, queryUniqueSymbols
	, manage
	, process
	, PortfolioT(..)
	, Portfolio(..)
	, manageSearch
	, testInsertPortfolio
	, testQueryPortfolios
	) where 
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core
import Data.Time

import Data.Either(rights)
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import Control.Exception
import qualified  Data.Map as IMap
import Control.Exception
import Control.Monad
import Control.Monad.Logger(runStderrLoggingT)
import Control.Monad.Trans.Maybe(runMaybeT)
import Control.Monad.Trans.State as State
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
import Data.Monoid (mappend, (<>) )
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
import qualified CCAR.Main.EnumeratedTypes as EnTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util as Util
import CCAR.Command.ApplicationError
import Database.Persist.Postgresql as Postgresql 
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))
import System.Log.Logger as Logger


--Helper functions 

iModuleName = "CCAR.Model.Portfolio"
queryPortfolio = "QueryPortfolio"

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
	pqCommandType :: T.Text
	, pqNickName :: T.Text
	, qCompanyId :: CompanyID 
	, qUserId :: INickName
	, resultSet :: [Either T.Text PortfolioT]
} deriving (Show, Read, Eq, Data, Generic, Typeable)

queryPortfolios :: PortfolioQuery -> IO (Either T.Text PortfolioQuery)
queryPortfolios query = do 
	dbOps $ do 
		company <- getBy $ UniqueCompanyId $ qCompanyId query 
		user <- getBy $ UniqueNickName $ qUserId query
		case (company, user) of 
			(Just (Entity cId _) , Just (Entity uId _)) -> do 
				companyUserId <- getBy $ UniqueCompanyUser cId uId
				case companyUserId of 
					Just (Entity cuId value) -> do 
						portfolios <- selectList [PortfolioCompanyUserId ==. cuId] []
						portfolioTs <- liftIO $ mapM (\(Entity k p) -> daoToDto Read k) portfolios
						return $ Right $ query {resultSet = portfolioTs}
					Nothing -> return $ Left $ "Query portfolios failed"


instance ToJSON PortfolioQuery where 
	toJSON pq@(PortfolioQuery cType nickName qCid userId r) = 
		object [
			"commandType" .= cType 
			, "nickName" .= nickName 
			, "companyId" .= qCid 
			, "useerId"  .= userId 
			, "resultSet" .= r
		]
instance FromJSON PortfolioQuery where 
	parseJSON (Object a)  = PortfolioQuery <$> 
						a .: "commandType" <*> 
						a .: "nickName" <*> 
						a .: "companyId" <*> 
						a .: "userId" <*> 
						a .: "resultSet"
	parseJSON _ 		 = Appl.empty

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
			, "commandType" .= ("ManagePortfolio" :: T.Text)
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

{-- Make sure that the dto and the dao refer to the same thing. To be implemented. --}
sanityCheck :: Key Portfolio -> PortfolioT -> Either T.Text Bool
sanityCheck a b = Right True

dtoToDao :: PortfolioT -> IO (Either T.Text Portfolio)
dtoToDao pT@(PortfolioT cType 
			porId comId userId summary createdBy updatedBy) = do 
	currentTime <- getCurrentTime 
	dbOps $ do 
			com <- getBy $ UniqueCompanyId comId 
			porKV <- getBy $ UniquePortfolio porId 
			case porKV of 
				Just (Entity pKey pValue) -> return $ Right pValue
				Nothing -> return $ Left $  "Portfolio id " `mappend` porId `mappend` " not found"


-- Optimisation note? or have we given up on database caches and let the db do its thing --
daoToDto :: CRUD -> PortfolioId -> IO (Either T.Text PortfolioT)
daoToDto crType pid = do 
	currentTime <- getCurrentTime 
	dbOps $ do
		porKV <- Postgresql.get pid 
		case porKV of 
			Just v -> do 
				comUser <- Postgresql.get (portfolioCompanyUserId v)
				case comUser of 
					Just coUser1 -> do 				
						company <- Postgresql.get (companyUserCompanyId coUser1)
						cUser <- Postgresql.get (companyUserUserId coUser1) 
						createdBy <- Postgresql.get (portfolioCreatedBy v) 
						updatedBy <- Postgresql.get (portfolioUpdatedBy v)
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


manageSearch :: NickName -> Value -> IO (GC.DestinationType, T.Text) 
manageSearch aNickName aValue@(Object a) = 
	case (fromJSON aValue) of 
		Success r -> do 
				res <- queryPortfolios r 
				case res of 
					Right x -> return (GC.Reply, serialize x) 
					Left y -> return (GC.Reply, serialize $ appError $ 
												"Manage query failed "  <> y)
		Error s ->  return (GC.Reply,
                     serialize $ appError $ "parse login  failed "++ s)


manage :: NickName -> Value -> IO (GC.DestinationType, T.Text)
manage aNickName aValue@(Object a) = 
	case (fromJSON aValue) of
		Success r -> do 
			res <- process r  
			case res of
				Right k -> do
					-- Delete wont find the object being deleted.
					case (crudType r) of 
						Delete -> do 
							reply <- return $ Right r 
							return (GC.Reply, serialize (reply :: Either T.Text PortfolioT))
						_ 		-> do  
							res1 <- daoToDto  (crudType r) k 
							case res1 of 
								Right pT -> 
									case (sanityCheck k pT) of 						
										Right _ -> return (GC.Reply, serialize res1)
										Left _ -> do 
											liftIO $ Logger.errorM iModuleName $ 
												("Input uuids and db uuids dont match "  :: String)
												`mappend` (show res)
											return (GC.Reply, serialize $ appError $
														T.unpack "Input output uuids dont match ")
								Left f2 -> 
									return (GC.Reply, serialize $ appError f2)
				Left f -> do 
					liftIO $ Logger.errorM iModuleName $ 
							"Error processing manage portfolio " `mappend` (show aValue)
					return (GC.Reply, serialize $ appError $ 
								"Error processing manage portfolio " ++ (T.unpack f))


process :: PortfolioT -> IO (Either T.Text (Key Portfolio))
process pT = case (crudType pT) of 
	Create -> insertPortfolio pT 
	Read -> readPortfolio pT -- single record
	P_Update -> updatePortfolio pT 
	Delete -> deletePortfolio pT 		


{-- | This gets all the symbols for a user registered in any company. Usually there should be 
only one such company. Admins or super users could probably share or regulators for example.--}
queryUniqueSymbols userId = dbOps $ do 
	x <- runMaybeT $ do 
		Just (Entity p _ ) <- lift $ getBy $ UniqueNickName userId 
		companies <- lift $ selectList [CompanyUserUserId ==. p] [] 
		s2 <- lift $ foldM (\a x@(Entity i val) ->  do 
						Just (cVal) <- Postgresql.get i
						company <- return $ companyUserCompanyId cVal 
						Just co <- Postgresql.get company
						companyId <- return $  companyCompanyID co 
						uniqSymbols <- liftIO $ queryUniqueSymbolsForCompany companyId userId
						return (uniqSymbols)) [] companies		
		return s2 
	case x of 
		Just y -> mapM (\x@(Entity k v) -> return v) y 
		
{-- | Return all unique symbols across all the portfolios for a user --}
--queryUniqueSymbols :: T.Text -> T.Text -> IO (Either T.Text [T.Text])
queryUniqueSymbolsForCompany companyId userId = dbOps $ do
	x <- runMaybeT $ do 
		Just (Entity c _) <- lift $ getBy $ UniqueCompanyId companyId 
		Just (Entity p _) <- lift $ getBy $ UniqueNickName userId 
		Just (Entity cu _) <- lift $ getBy $ UniqueCompanyUser c p
		portfolios <- lift $ selectList [(PortfolioCompanyUserId ==. cu)][]
		s2 <- foldM (\a x@(Entity b _)-> do 
				n <- lift $ selectList [PortfolioSymbolPortfolio ==. b] [] 
				return ( n `mappend` a) 
					) [] portfolios
		return s2
	case x of 
		Just y -> return y
		Nothing -> return []


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
				c <- getBy $ UniqueCompanyId companyId 
				nPerson <- getBy $ UniqueNickName userId 
				crBy <- getBy $ UniqueNickName createdBy
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
-- To run: source config/dev.env 
-- cabal repl 
-- :l Portfolio.hs
-- >testInsertPortfolio -- it should work most of the times.
testInsertPortfolio = do 
	-- Create a person
	uuid <- nextUUID 
	currentTime <- getCurrentTime
	case uuid of 
		Just u -> do 
			person <- dbOps $ insert $ Person "portfolioTest" "portfolioTest" 
								(T.pack $ uuidAsString u) "portfolioTest" (Just "en-us") currentTime
			companyUUID <- nextUUID 
			case companyUUID of 
				Just cU -> do 
					currentTime <- getCurrentTime
					company <- dbOps $ insert $ Company "PortfolioTester" (T.pack $ uuidAsString cU) 
													"tester@portfoliotester.com" 
													"No image" 
													person 
													currentTime 
													currentTime

					--Create a company user
					companyUser <- dbOps $ insert $ CompanyUser company person chatMinder support locale
					portfolio <- insertPortfolio $ PortfolioT Create "insert_me" 
							(uuidAsText cU) 
							userIdAsText "Test portfolio" 
							userIdAsText
							userIdAsText
					return portfolio
					where 
							uuidAsText = T.pack . uuidAsString 
							userIdAsText = uuidAsText u
	where
		chatMinder = True
		support = True
		locale = Just ("en-us")


testQueryPortfolios = do 
	portfolio <- testInsertPortfolio
	case portfolio of 
		Right pID -> do 
			pEntity <- dbOps $ Postgresql.get pID 
			case pEntity of 
				Just pEV -> do
					user <- dbOps $ Postgresql.get (portfolioCreatedBy pEV)
					companyUser <- dbOps $ Postgresql.get (portfolioCompanyUserId pEV)
					case (user, companyUser) of 
						(Just u, Just cu) -> do
								com <- dbOps $ Postgresql.get (companyUserCompanyId cu)
								case com of 
									Just com1 -> do    
										queryPortfolios $ PortfolioQuery queryPortfolio 
												(personNickName u) 
												(companyCompanyID com1)
												(personNickName u) 
												[]


instance ToJSON CRUD 
instance FromJSON CRUD 
instance ToJSON PortfolioCommands 
instance FromJSON PortfolioCommands
instance ToJSON PortfolioSymbolTypeQuery
instance FromJSON PortfolioSymbolTypeQuery 
instance ToJSON PortfolioSymbolSideQuery where
	toJSON p2@(PortfolioSymbolSideQuery a b c) =
		object [
			"nickName" .= a 
			, "commandType" .= b 
			, "symbolSides" .= c
		]

instance FromJSON PortfolioSymbolSideQuery where
	parseJSON (Object a) = PortfolioSymbolSideQuery <$>
					a .: "nickName" <*> 
					a .: "commandType" <*>
					a .: "symbolSides"
	parseJSON _ 		= Appl.empty