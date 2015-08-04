module CCAR.Model.PortfolioSymbol (
	manage
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



iModuleName = "CCAR.Model.PortfolioSymbol"
managePortfolioSymbolCommand = "ManagePortfolioSymbol"

data CRUD = Create | Read | P_Update | Delete 
			deriving (Show, Read, Eq, Data, Generic, Typeable)
			

data PortfolioSymbolT = PortfolioSymbolT {
	  crudType :: CRUD
	, commandType :: T.Text 
	, portfolioID :: T.Text -- unique uuid for the portfolio
	, symbol :: T.Text 
	, quantity :: Double 
	, side :: EnTypes.PortfolioSymbolSide 
	, symbolType :: EnTypes.PortfolioSymbolType 
	, createdBy :: T.Text 
	, updatedBy :: T.Text 
	, pSTNickName :: T.Text
} deriving (Show, Read, Eq, Data, Generic, Typeable)

instance ToJSON PortfolioSymbolT where 
	toJSON pS1@(PortfolioSymbolT crType coType portId symbol quantity side symbolType cr up nickName)= 
			object [
				"crudType" .= crType
				, "commandType" .= coType 
				, "portfolioId" .= portId 
				, "symbol" .= symbol 
				, "quantity" .= quantity
				, "side" .= side 
				, "symbolType" .= symbolType 
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
											pS ) portfolioSymbolList
						return $ Right $ p {resultSet = portfolioSymbolListT}


dtoToDao :: PortfolioSymbolT -> IO PortfolioSymbol 
dtoToDao = undefined


daoToDto :: CRUD -> T.Text -> T.Text -> T.Text -> T.Text -> PortfolioSymbol -> (Either T.Text PortfolioSymbolT) 
daoToDto crudType pUUID creator updator currentRequest 
			p@(PortfolioSymbol pID symbol quantity side symbolType cB cT uB uT )  = 
				Right $ PortfolioSymbolT crudType
								managePortfolioSymbolCommand 
								pUUID symbol quantity side symbolType 
								creator updator currentRequest




-- create, read , update and delete operations
manage :: NickName -> Value -> IO (GC.DestinationType, T.Text)
manage aNickName aValue@(Object a) = 
	case (fromJSON aValue) of
		Success r -> do 
			res <- process r  
			case res of
				Right (k, (creator, updator, portfolioUUID)) -> do 
					portfolioEntity <- dbOps $ get k 
					case portfolioEntity of 
						Just pEVa -> do 
							res1 <- return $ daoToDto (crudType r) portfolioUUID creator updator aNickName pEVa 
							case res1 of 
								Right pT -> return (GC.Reply, serialize res)
								Left f -> do 
									liftIO $ Logger.errorM iModuleName $ 
										"Error processing manage portfolio " `mappend` (show aValue)
									return (GC.Reply, serialize $ genericErrorCommand $ 
										"Error processing manage portfolio " ++ (T.unpack f))
				Left p2 -> do
							liftIO $ Logger.errorM iModuleName $ 
								"Error processing manage portfolio " `mappend` (show aValue)
							return (GC.Reply, serialize $ genericErrorCommand $ 
								"Error processing manage portfolio " ++ (T.unpack p2))


process :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
process pT = case (crudType pT) of 
	Create -> insertPortfolioSymbol pT 
	Read -> readPortfolioSymbol pT -- single record
	P_Update -> updatePortfolioSymbol pT 
	Delete -> deletePortfolioSymbol pT 		

insertPortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
insertPortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType 
								creator
								updator
								requestor			
						)
						 = dbOps $ do 
				portfolio <- getBy $ UniquePortfolio portfolioId 
				currentTime <- liftIO $ getCurrentTime
				case portfolio of 
					Just (Entity pID pValue) -> do 
						cr <- getBy $ PersonUniqueNickName creator
						up <- getBy $ PersonUniqueNickName updator 
						req <- getBy $ PersonUniqueNickName requestor 
						case (cr, up, req) of 
							(Just (Entity crID crValue), Just (Entity upID upValue), Just (Entity reqID reqValue)) -> do 
									n <- insert $ PortfolioSymbol pID symbol quantity side symbolType crID currentTime upID currentTime
									return $ Right (n, (creator, updator, portfolioId))
							_ -> do 
								liftIO $ Logger.errorM iModuleName $ 
											"Error processing manage portfolio symbol " `mappend` (show a)
								return $ Left $ T.pack $ "Insert failed " `mappend` (T.unpack portfolioId)
					Nothing -> return $ Left $ T.pack $ "Portfolio not found " `mappend` (T.unpack portfolioId)

updatePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
updatePortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType 
								creator
								updator
								requestor) = dbOps $ do 
		portfolio <- getBy $ UniquePortfolio portfolioId 
		currentTime <- liftIO $ getCurrentTime
		case portfolio of 
			Just (Entity pID pValue) -> do 
				portfolioSymbol <- getBy $ UniquePortfolioSymbol pID symbol symbolType side 
				case portfolioSymbol of 
					Just (Entity psID pValue) -> do 
						liftIO $ Logger.debugM iModuleName $ 
								"Updating portfolio symbol " `mappend` (T.unpack portfolioId)
										`mappend` " " `mappend` (show a)
						x <- update psID [PortfolioSymbolQuantity =. quantity
										   , PortfolioSymbolUpdatedOn =. currentTime]
						return $ Right (psID, (creator, updator, portfolioId))

deletePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
deletePortfolioSymbol a@(PortfolioSymbolT crType commandType 
								portfolioId 
								symbol 
								quantity
								side 
								symbolType 
								creator
								updator
								requestor) = dbOps $ do
	portfolio <- getBy $ UniquePortfolio portfolioId
	case portfolio of 
		Just (Entity pID pValue) -> do 
			portfolioSymbol <- getBy $ UniquePortfolioSymbol pID symbol symbolType side 
			case portfolioSymbol of 
				Just (Entity psID pValue) -> do 
					liftIO $ Logger.debugM iModuleName $ "Deleting portfolio symbol " `mappend` (show a)
					delete psID 
					return $ Right (psID, (creator, updator, portfolioId))
				Nothing -> do 
					liftIO $ Logger.errorM iModuleName $ "Portfolio symbol not found " `mappend` (show a) 
					return $ Left $ T.pack $ "Error deleting " `mappend` (show a)



readPortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol, (T.Text, T.Text, T.Text)))
readPortfolioSymbol = undefined


instance ToJSON CRUD 
instance FromJSON CRUD




