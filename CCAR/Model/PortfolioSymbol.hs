module CCAR.Model.PortfolioSymbol (
	manage
	, readPortfolioSymbol
	, manageSearch
	, daoToDto
	, testInsert
	, testInsertNew 
	, CRUD(..)
	, PortfolioSymbolT(..)
	) where 
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core

import Control.Monad.Reader
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import Control.Exception
import qualified  Data.Map as IMap
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Logger(runStderrLoggingT)
import Network.WebSockets.Connection as WSConn
import Data.Text as T
import Data.Text.Lazy as L 

import Data.Aeson.Encode as En
import Data.Text.Lazy.Encoding as E
import Data.Aeson as J
import Data.HashMap.Lazy as LH (HashMap, lookup)
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)

import Database.Persist 
import Database.Persist.Postgresql as DB
import Database.Persist.TH

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
import CCAR.Command.ApplicationError
import Database.Persist.Postgresql as Postgresql 
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))
import System.Log.Logger as Logger



iModuleName = "CCAR.Model.PortfolioSymbol"
managePortfolioSymbolCommand = "ManagePortfolioSymbol"
manageSearchPortfolioCommand = "QueryPortfolioSymbol"
data CRUD = Create | Read | P_Update | Delete 
			deriving (Show, Read, Eq, Data, Generic, Typeable)
			

data PortfolioSymbolT = PortfolioSymbolT {
	  crudType :: CRUD
	, commandType :: T.Text 
	, portfolioID :: T.Text -- unique uuid for the portfolio
	, symbol :: T.Text 
	, quantity :: T.Text 
	, side :: EnTypes.PortfolioSymbolSide 
	, symbolType :: EnTypes.PortfolioSymbolType 
	, value :: T.Text
	, stressValue :: T.Text
	, createdBy :: T.Text 
	, updatedBy :: T.Text 
	, pSTNickName :: T.Text
} deriving (Show, Read, Eq, Data, Generic, Typeable)


instance ToJSON PortfolioSymbolT where 
	toJSON pS1@(PortfolioSymbolT crType coType portId symbol quantity side symbolType value 
						sVal cr up nickName)= 
			object [
				"crudType" .= crType
				, "commandType" .= coType 
				, "portfolioId" .= portId 
				, "symbol" .= symbol 
				, "quantity" .= quantity
				, "side" .= side 
				, "symbolType" .= symbolType 
				, "value" .= value
				, "stressValue" .= sVal
				, "creator" .= cr 
				, "updator" .= up 
				, "nickName" .= nickName
			]

instance FromJSON PortfolioSymbolT where 
	parseJSON (Object a ) = PortfolioSymbolT <$> 
			a .: "crudType" <*>
			a .: "commandType" <*>
			a .: "portfolioId" <*> 
			a .: "symbol" <*> 
			a .: "quantity" <*>
			a .: "side" <*> 
			a .: "symbolType" <*>
			a .: "value" <*>
			a .: "stressValue" <*>
			a .: "creator" <*> 
			a .: "updator" <*>
			a .: "nickName" 

	parseJSON _ = Appl.empty




data PortfolioSymbolQueryT = PortfolioSymbolQueryT {
	qCommandType :: T.Text 
	, qPortfolioID :: T.Text
	, resultSet :: [Either T.Text PortfolioSymbolT]
	, psqtNickName :: T.Text 
} deriving (Show, Read, Eq, Data, Generic, Typeable)

instance ToJSON PortfolioSymbolQueryT where 
	toJSON qp@(PortfolioSymbolQueryT cType pID rS nickName) = 
			object [
				"nickName" .= nickName
				, "portfolioId" .= pID
				, "commandType" .= cType
				, "resultSet" .= rS
			]
instance FromJSON PortfolioSymbolQueryT where
	parseJSON (Object a) = PortfolioSymbolQueryT <$> 
								a .: "commandType" <*> 
								a .: "portfolioId" <*> 
								a .: "resultSet" <*> 
								a .: "nickName"
	parseJSON _ 	= Appl.empty

queryPortfolioSymbol :: PortfolioSymbolQueryT -> IO (Either T.Text PortfolioSymbolQueryT) 
queryPortfolioSymbol p@(PortfolioSymbolQueryT cType 
						pUUID 
						resultSet
						nickName) = dbOps $ do 
				portfolio <- getBy $ UniquePortfolio pUUID 
				case portfolio of 
					Just (Entity pID pValue) -> do 
						portfolioSymbolList <- selectList [PortfolioSymbolPortfolio ==. pID] []
						portfolioSymbolListT  <- liftIO $ mapM (\(Entity k pS) -> dbOps $ do 
								creator <- get $ portfolioSymbolCreatedBy pS  
								updator <- get $ portfolioSymbolUpdatedBy pS  
								case(creator, updator) of 
									(Just cr, Just upd ) -> 
										return $ daoToDto Read 
											pUUID 
											(personNickName cr) 
											(personNickName upd)
											nickName
											pS "0.0") portfolioSymbolList
						return $ Right $ p {resultSet = portfolioSymbolListT}


dtoToDao :: PortfolioSymbolT -> IO PortfolioSymbol 
dtoToDao = undefined


daoToDto :: CRUD -> T.Text -> T.Text -> T.Text -> T.Text -> PortfolioSymbol -> T.Text -> (Either T.Text PortfolioSymbolT) 
daoToDto crudType pUUID creator updator currentRequest 
			p@(PortfolioSymbol pID symbol quantity side symbolType value cB cT uB uT ) sVal = 
				Right $ PortfolioSymbolT crudType
								managePortfolioSymbolCommand 
								pUUID symbol (quantity) side symbolType
								value sVal
								creator updator currentRequest


manageSearch :: NickName -> Value -> IO (GC.DestinationType, T.Text) 
manageSearch aNickName aValue@(Object a) = 
	case (fromJSON aValue) of 
		Success r -> do 
				result <- queryPortfolioSymbol r 
				return (GC.Reply, serialize result) 
		Error s -> return (GC.Reply, serialize $ appError $
							"Error processing manage search for portfolio symbol: "  ++ s)


-- create, read , update and delete operations
manage :: NickName -> Value -> IO (GC.DestinationType, T.Text)
manage aNickName aValue@(Object a) = 
	case (fromJSON aValue) of
		Success r -> do 
			res <- process r  
			case res of
				Right (k, (creator, updator, portfolioUUID)) -> do 
					case (crudType r) of 
						Delete -> do 
							reply <- return $ Right r 
							return (GC.Reply, serialize (reply :: Either T.Text PortfolioSymbolT))
						_ 	   -> do 
							portfolioEntity <- dbOps $ get k 
							case portfolioEntity of 
								Just pEVa -> do 
									res1 <- return $ daoToDto (crudType r) 
										portfolioUUID creator updator aNickName pEVa "0.0"
									case res1 of 
										Right pT -> return (GC.Reply, serialize res1)
										Left f -> do 
											liftIO $ Logger.errorM iModuleName $ 
												"Error processing manage portfolio " `mappend` (show aValue)
											return (GC.Reply, serialize $ appError $ 
												"Error processing manage portfolio " ++ (T.unpack f))
				Left p2 -> do
							liftIO $ Logger.errorM iModuleName $ 
								"Error processing manage portfolio " `mappend` (show aValue)
							return (GC.Reply, serialize $ appError $ 
								"Error processing manage portfolio " ++ (T.unpack p2))
		Error s -> 
				return (GC.Reply, serialize $ 
							appError $ 
								"Error processing manage portfolio symbol " ++ s)

process :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
process pT = case (crudType pT) of 
	Create -> insertPortfolioSymbol pT 
	Read -> readPortfolioSymbol pT -- single record
	P_Update -> updatePortfolioSymbol pT 
	Delete -> deletePortfolioSymbol pT 		

type P_Creator = T.Text
type P_Updator = T.Text 
type P_PortfolioId = T.Text
insertPortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (P_Creator, P_Updator, P_PortfolioId)))
insertPortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType
								value
								sVal 
								creator
								updator
								requestor			
						)
						 = do 
				portfolioSymbol <- liftIO $ readPortfolioSymbol a 
				case portfolioSymbol of 
					Right _ -> do 
						liftIO $ Logger.errorM iModuleName 
								$ "Portfolio symbol exists. Updating the record, because we have the record:"
									`mappend` (show a)
						updatePortfolioSymbolI portfolioSymbol a 
					Left _ -> dbOps $ do 
						portfolio <- getBy $ UniquePortfolio portfolioId 
						currentTime <- liftIO $ getCurrentTime
						case portfolio of 
							Just (Entity pID pValue) -> do 
								cr <- getBy $ UniqueNickName creator
								up <- getBy $ UniqueNickName updator 
								req <- getBy $ UniqueNickName requestor 
								case (cr, up, req) of 
									(Just (Entity crID crValue), Just (Entity upID upValue), Just (Entity reqID reqValue)) -> do 
											n <- insert $ PortfolioSymbol pID symbol 
														(quantity) 
														side symbolType 
														"0.0"
														crID currentTime upID currentTime
											return $ Right (n, (creator, updator, portfolioId))
									_ -> do 
										liftIO $ Logger.errorM iModuleName $ 
													"Error processing manage portfolio symbol " `mappend` (show a)
										return  $ Left $ T.pack $ "Insert failed " `mappend` (T.unpack portfolioId)
							Nothing -> return $ Left $ T.pack $ "Portfolio not found " `mappend` 
																(T.unpack portfolioId)

updatePortfolioSymbolI portfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType
								value
								sVal  
								creator
								updator
								requestor) = dbOps $ do 
			currentTime <- liftIO $ getCurrentTime
			case portfolioSymbol of 
				Right (psID, _) -> do 
								x <- update psID [PortfolioSymbolQuantity =. (read $ T.unpack quantity)
											   , PortfolioSymbolUpdatedOn =. currentTime]
								return $ Right (psID, (creator, updator, portfolioId))
				Left x -> do 
					liftIO $ Logger.errorM iModuleName $ "Error updating portfolio symbol " `mappend` (show a) 
					return portfolioSymbol

updatePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
updatePortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType 
								value
								sVal 
								creator
								updator
								requestor) = dbOps $ do 
		portfolioSymbol <- liftIO $ readPortfolioSymbol a 
		currentTime <- liftIO $ getCurrentTime
		case portfolioSymbol of 
			Right (psID, _) -> do 
							x <- update psID [PortfolioSymbolQuantity =. (read $ T.unpack quantity)
										   , PortfolioSymbolUpdatedOn =. currentTime]
							return $ Right (psID, (creator, updator, portfolioId))
			Left x -> do 
				liftIO $ Logger.errorM iModuleName $ "Error updating portfolio symbol " `mappend` (show a) 
				return portfolioSymbol

deletePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
deletePortfolioSymbol a = dbOps $ do
	portfolioSymbol <- liftIO $ readPortfolioSymbol a 
	case portfolioSymbol of 
		Right (psID, _) -> do 
			liftIO $ Logger.debugM iModuleName $ "Deleting portfolio symbol " `mappend` (show a) 
			delete psID 
			return portfolioSymbol 
		Left x -> do 
			liftIO $ Logger.errorM iModuleName $ "Error deleting portfolio symbol " `mappend` (show a)
			return portfolioSymbol


readPortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
readPortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType 
								value
								sVal  
								creator
								updator
								requestor) = dbOps $ do 
	portfolio <- getBy $ UniquePortfolio portfolioId
	case portfolio of 
		Just (Entity pID pValue) -> do 
			portfolioSymbol <- getBy $ UniquePortfolioSymbol pID symbol symbolType side 
			case portfolioSymbol of 
				Just (Entity psID pValue) -> do 
					liftIO $ Logger.debugM iModuleName $ "Reading portfolio symbol " `mappend` (show a)
					return $ Right (psID, (creator, updator, portfolioId))
				Nothing -> do 
					liftIO $ Logger.errorM iModuleName $ "Portfolio symbol not found " `mappend` (show a) 
					return $ Left $ T.pack $ "Error reading " `mappend` (show a)

uuidAsString = UUID.toString 



{-- | This method as one could no doubt notice is a much better representation of the same code. --}
testInsert index portfolioID = dbOps $ do 
	x <- runMaybeT $ do 
		Just u <- liftIO nextUUID 
		currentTime <- liftIO getCurrentTime
		Just portfolio <- lift $ get portfolioID 
		Just companyUser <- lift $ get $ portfolioCompanyUserId portfolio 
		Just user <- lift $ get $ companyUserUserId companyUser 
		liftIO $ insertPortfolioSymbol $ PortfolioSymbolT Create 
												managePortfolioSymbolCommand
												(portfolioUuid portfolio)
												{-("ABC" `mappend` (T.pack $ show index))-}
												"ABC"
												"314.14"
												EnTypes.Buy
												EnTypes.Equity
												"0.0"
												"0.0"
												(personNickName user)
												(personNickName user)
												(personNickName user)
	case x of 
		Just x -> return x 
		Nothing -> return $ Left $ "testInsert failed"
{-- | This method is mainly as an example of how non monadic code can create the dreaded 
staircase. --}
testInsertNonM :: Integer -> Key Portfolio -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text))) 
testInsertNonM index portfolioID = dbOps $ do
	u <- liftIO $ nextUUID
	currentTime <- liftIO $ getCurrentTime
	portfolio <- get portfolioID 
	case (portfolio, u) of 
		(Just por, Just uuid) ->  do 
			companyUserE <- get $ portfolioCompanyUserId por 
			case companyUserE of 
				Just cuE -> do 
						user <- get $ companyUserUserId cuE
						case user of 
							Just userFound -> 
								liftIO $ insertPortfolioSymbol $ PortfolioSymbolT Create 
												managePortfolioSymbolCommand
												(portfolioUuid por)
												{-("ABC" `mappend` (T.pack $ show index))-}
												"ABC"
												"314.14"
												EnTypes.Buy
												EnTypes.Equity
												"0.0"
												"0.0"
												(personNickName userFound)
												(personNickName userFound)
												(personNickName userFound)
		_ -> return $ Left $ "testInsert failed"										

testInsertNew index pId = do 
	xo <- dbOps $ do 
		x <- runMaybeT $ do 
				u <- liftIO nextUUID 			
				currentTime <- liftIO $ getCurrentTime
				Just portfolio <- lift $ get pId 
				Just companyUser <- lift $ get $ portfolioCompanyUserId portfolio 
				Just user <- lift $ get $ companyUserUserId companyUser 
				Just (Entity userId uIgnore) <- lift $ getBy $ UniqueNickName $ personNickName user 
				lift $ insert $ 
							PortfolioSymbol pId
								("ABC" `mappend` (T.pack $ show index))
								"314.14"
								EnTypes.Buy
								EnTypes.Equity
								"0.0"
								userId 
								currentTime
								userId
								currentTime

		return x 
	return xo
instance ToJSON CRUD 
instance FromJSON CRUD




