module CCAR.Data.TradierApi 
	(startup, query, QueryOptionChain, queryMarketData)
where 

import CCAR.Main.DBOperations (Query, query, manage, Manager)
import Network.HTTP.Conduit 

-- The streaming interface
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (Value (Object, String, Array) 
										  , toJSON 
										  , fromJSON)
import           Data.Aeson              (encode, object, (.=), (.:), decode
											, parseJSON)
import           Data.Aeson.Parser       (json)
import			 Data.Aeson.Types 		 (parse, ToJSON, FromJSON, toJSON)
import           Data.Conduit            (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import           Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody, withManager)
import 			CCAR.Parser.CSVParser as CSVParser(parseCSV, ParseError, parseLine)
import			Control.Concurrent(threadDelay)
import 			Data.Text as T 
import 			Data.Text.Lazy as L
import 			Data.ByteString.Lazy as LB 
import			Data.Text.Lazy.Encoding as LTE
import 			Data.Aeson.Types	 as AeTypes(Result(..), parse)
import 			Data.ByteString.Internal 	 as S
import			Data.HashMap.Strict 		 as M 
import 			Control.Exception hiding(Handler)

import 			Database.Persist
import 			Database.Persist.TH 

import 			qualified CCAR.Main.GroupCommunication as GC
import 			Database.Persist.Postgresql as DB
import 			Data.Time
import			Data.Map
import Control.Monad.IO.Class 
import Control.Monad
import Control.Monad.Logger 
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Applicative as Appl
import Data.Monoid(mappend, (<>))
import CCAR.Main.DBUtils
import CCAR.Command.ApplicationError(appError)

import System.Environment(getEnv)
import GHC.Generics
import GHC.IO.Exception
import Data.Vector
import Data.Scientific
import Data.Data
import Data.Monoid (mappend)
import Data.Typeable 
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe(runMaybeT)
import Control.Monad.Trans(lift)
import System.Log.Logger as Logger
import Data.Conduit.Binary as B (sinkFile, lines, sourceFile) 
import Data.Conduit.List as CL 
import Data.ByteString.Char8 as BS(ByteString, pack, unpack) 
import Data.Conduit ( ($$), (=$=), (=$), Conduit, await, yield)

iModuleName = "CCAR.Data.TradierApi"
baseUrl =  "https://sandbox.tradier.com/v1"

quotesUrl =  "markets/quotes"
timeAndSales = "markets/timesales"
optionChains = "markets/options/chains"
provider = "Tradier"

historicalMarketData = "markets/history"

insertTradierProvider = 
	dbOps $ do
		get <- DB.getBy $ UniqueProvider provider 
		y <- case get of 
				Nothing -> do 
					DB.insert $ MarketDataProvider 
							provider baseUrl "" timeAndSales optionChains 
		return y 


getMarketData url queryString = do 
	authBearerToken <- getEnv("TRADIER_BEARER_TOKEN") >>= 
		\x ->  return $ S.packChars $ "Bearer " `mappend` x
	runResourceT $ do 
		manager <- liftIO $ newManager tlsManagerSettings 

		req <- liftIO $ parseUrl makeUrl
		req <- return $ req {requestHeaders = 
				[("Accept", "application/json") 
				, ("Authorization", 
					authBearerToken)]
				}
		req <- return $ setQueryString queryString req 
		res <- http req manager 
		value <- responseBody res $$+- sinkParser json 
		return value 
	where 
		makeUrl = S.unpackChars $ LB.toStrict $ LB.intercalate "/" [baseUrl, url]


getQuotes = \x  -> getMarketData quotesUrl [("symbols", Just x)] 

getTimeAndSales y = \x -> getMarketData timeAndSales [("symbol", Just x)]


getHistoricalData = \x -> do 
	Object value <- getMarketData historicalMarketData [("symbol", Just x)]
	history <- return $ M.lookup "history" value
	case history of 
		Just aValue -> do 
			case aValue of 
				a@(Array x) -> return $ Right $ fmap (\y -> fromJSON y :: Result MarketData) x 
				_			-> return $ Left $ "Error processing " `mappend` (show aValue)

getOptionChains = \x y -> do 
	liftIO $ Logger.debugM iModuleName ("Inside option chains " `mappend` (show x) `mappend` (show y))
	Object value <- getMarketData optionChains [("symbol", Just x), ("expiration", Just y )]
	liftIO $ Logger.debugM iModuleName ("getOptionChains " `mappend` (show x))
	options <- return $ M.lookup "options" value 
	case options of 
		Just (Object object) -> do 
			object <- return $ M.lookup "option" object
			case object of 
				Just aValue -> do
					case aValue of 
						a@(Array x) ->  do  
							return $ Right $ fmap (\y -> fromJSON y :: Result OptionChainMarketData) x 
						_ -> return $ Left $ "Error processing" `mappend` (show y)
				_ -> return $ Left $ "Error processing " `mappend` (show y)
		_ -> return $ Left "Nothing to process"
		

insertOptionChain x = dbOps $ do 
	liftIO $ Logger.debugM iModuleName ("inserting single " `mappend` (show x))
	x <- runMaybeT $ do 
		Just (Entity kId providerEntity) <- 
				lift $ DB.getBy $ UniqueProvider provider
		liftIO $ Logger.debugM iModuleName $ "Entity  " `mappend` (show providerEntity)
   		lift $ DB.insert $ OptionChain  
				(symbol x)
				(T.pack $ show $ strike x)				
				(T.pack $ show $ lastPrice x)
				(T.pack $ show $ bidPrice x) 
				(T.pack $ show $ askPrice x)
				(T.pack $ show $ change x)
				(T.pack $ show $ openInterest x)
				(underlying x)
				(expiration x)				
				(optionType x)
				(kId)
	return x	

insertHistoricalPrice y = dbOps $ do 
	x <- runMaybeT $ do 
		Just (Entity kId providerEntity) <- lift $ DB.getBy $ UniqueProvider provider
		lift $ DB.insert $ y {marketDataDataProvider = kId}
	return x


data QueryOptionChain = QueryOptionChain {
	qNickName :: T.Text
	, qCommandType :: T.Text
	, qUnderlying :: T.Text
	, optionChain :: [OptionChain] 
} deriving (Show, Eq)


parseQueryOptionChain v = QueryOptionChain <$> 
				v .: "nickName" <*>
				v .: "commandType" <*> 
				v .: "underlying" <*> 
				v .: "optionChain"

genQueryOptionChain (QueryOptionChain n c underlying res) = 
		object [
			"nickName" .= n 
			, "commandType" .= c 
			, "underlying" .= underlying 
			, "optionChain" .= res 

		]


instance ToJSON QueryOptionChain where 
	toJSON = genQueryOptionChain 

instance FromJSON QueryOptionChain where
	parseJSON (Object v) = parseQueryOptionChain v 
	parseJSON _ 		 = Appl.empty

instance Query QueryOptionChain where 
	query = queryOptionChain

data OptionChainMarketData = OptionChainMarketData {
		symbol :: T.Text 
		, underlying :: T.Text 
		, strike :: Scientific
		, expiration :: T.Text
		, optionType :: T.Text
		, lastPrice :: Maybe Scientific
		, bidPrice :: Maybe Scientific
		, askPrice :: Maybe Scientific
		, change :: Maybe Scientific
		, openInterest :: Maybe Scientific
	}deriving (Show, Eq, Data, Generic, Typeable)


instance FromJSON OptionChainMarketData where 
	parseJSON (Object o) = OptionChainMarketData <$> 
								o .: "symbol" <*>
								o .: "root_symbol" <*> 
								o .: "strike" <*>
								o .: "expiration_date" <*> 
								o .: "option_type" <*>
								o .: "last" <*> 
								o .: "bid" <*> 
								o .: "ask" <*> 
								o .: "change" <*> 
								o .: "open_interest"
	parseJSON _ 		 = Appl.empty
instance ToJSON OptionChainMarketData
						



{-- | Returns the option expiration date for n months from now. --}
expirationDate n = do 

	x <- liftIO $ getCurrentTime >>= \l 
				-> return $ utctDay l
	(yy, m, d) <- return $ toGregorian x
	-- Compute the number of days
	y <- Control.Monad.foldM (\a b -> 
				return $ 
					a + (gregorianMonthLength yy (m + b)))
				0 [0..n]
	x2 <- return $ addDays (toInteger y) x
	(yy2, m2, d2) <- return $ toGregorian x2 
	monthLength <- return $ toInteger $ gregorianMonthLength yy m2
	res <- return $ addDays (monthLength - (toInteger d2)) x2
	return $ BS.pack $ show res

defaultExpirationDate = expirationDate 0


insertDummyMarketData = dbOps $ do
	time <- liftIO $ getCurrentTime 
	y <- runMaybeT $ do 
		x <- lift $ selectList [][Asc EquitySymbolSymbol]
		Just (Entity kid providerEntity) <- 
				lift $ DB.getBy $ UniqueProvider provider
		y <- Control.Monad.mapM (\a @(Entity k val) -> do 
				lift $ DB.insert $ MarketData (equitySymbolSymbol val) 
									time 
									"1.0"
									"1.0"
									"1.0"
									"1.0"
									"1.0"
									time 
									kid
				newTime <- liftIO $ return $ addUTCTime (24 * 3600) time
				lift $ DB.insert $ MarketData (equitySymbolSymbol val) 
									newTime
									"3.0"
									"3.0"
									"3.0"
									"3.0"
									"3.0"
									time
									kid
									) x 

		return () 
	return y

queryMarketData :: IO (Map T.Text MarketData)
queryMarketData = dbOps $ do 
		-- A bit of a hack. Sort by ascending market data date to replace with the latest element.
		x <- selectList [][Asc MarketDataSymbol, Asc MarketDataDate]
		y <- Control.Monad.mapM (\a@(Entity k val) -> return (marketDataSymbol val, val)) x 
		return $ Data.Map.fromList y 
--TODO: Exception handling needs to be robust.
insertAndSave :: [String] -> IO (Either T.Text T.Text)
insertAndSave x = (dbOps $ do
	symbol <- return $ T.pack $ x !! 0 
	symbolExists <- getBy $ UniqueEquitySymbol symbol 
	expirationDate <- liftIO $ defaultExpirationDate
	case symbolExists of 
		Nothing ->  do
				i <- DB.insert $ EquitySymbol symbol
							(T.pack $ x !! 1)
							(T.pack $ x !! 2) 
							(T.pack $ x !! 3)
							(T.pack $ x !! 4)
							(read (x !! 5) :: Int )
				liftIO $ Logger.debugM iModuleName ("Insert succeeded " `mappend` (show i))
				return $ Right symbol
		Just x -> return $ Right symbol 
				)
				`catch`
				(\y -> do 
						Logger.errorM iModuleName $ 
							"Failed to insert " `mappend` (show (y :: SomeException))
						return $ Left "ERROR")

	
parseSymbol :: (Monad m, MonadIO m) => Conduit BS.ByteString m (Either ParseError [String])
parseSymbol = do 
	client <- await 
	case client of 
		Nothing -> return () 
		Just aBS -> do 
				yield $ CSVParser.parseLine $ BS.unpack aBS
				parseSymbol


saveSymbol :: (MonadIO m) => Conduit (Either ParseError [String]) m (Either T.Text T.Text)
saveSymbol = do 
	client <- await 
	case client of 
		Nothing -> return () 
		Just oString -> do 
			case oString of 				
				Right x -> do 
					x <- liftIO $ insertAndSave x 										
					yield x 
					return x
			
			saveSymbol

saveHistoricalData :: (MonadIO m) => Conduit (Either T.Text T.Text) m (Either T.Text T.Text)
saveHistoricalData = do 
	client <- await 
	case client of
		Just (Right x) -> do 
			_ <- liftIO $ insertHistoricalIntoDb (T.unpack x)
			yield $ Right x
			return $ Right x 
		Nothing -> do 
			yield $ Left $ T.pack $ "Nothing to save "
			return $ Left $ "Nothing to save "
	saveHistoricalData 


queryOptionChain aNickName o = do 
	x <- case (parse parseJSON o :: Result QueryOptionChain) of 
		Success r@(QueryOptionChain ni cType underlying _) -> do 
			optionChainE <- dbOps $ selectList [OptionChainUnderlying ==. underlying] []
			optionChain <- Control.Monad.forM optionChainE (\(Entity id x) -> return x)
			return $ Right $ 
				QueryOptionChain ni cType underlying optionChain
		Error s ->  return $ Left $ appError $ "Query users for a company failed  " `mappend` s
	return (GC.Reply, x)

saveOptionChains :: (MonadIO m) => Conduit (Either T.Text T.Text) m BS.ByteString
saveOptionChains = do 
	symbol <- await
	liftIO $ threadDelay (10^6) -- a second delay  
	case symbol of 
		Nothing -> return () 
		Just x -> do 
			liftIO $ Logger.debugM iModuleName ("Saving option chains for " `mappend` (show x))
			case x of 
				(Right aSymbol) -> do 
					liftIO $ Logger.debugM iModuleName ("Inserting into db " `mappend` (show x))
					d <- defaultExpirationDate
					i <- liftIO $ insertOptionChainsIntoDb (BS.pack $ T.unpack aSymbol) d
					yield $ BS.pack $ "Option chains for " `mappend` 
									(T.unpack aSymbol) `mappend` " retrieved: "
									`mappend` (show i)					
			 	(Left aSymbol) -> do 
			 		liftIO $ Logger.errorM iModuleName $ "Not parsing symbol"  `mappend` (show aSymbol)
			 		yield $ BS.pack $ "Option chain not parsed for " `mappend` (show aSymbol)
			saveOptionChains



insertHistoricalIntoDb x = do 
	x1 <- getHistoricalData (BS.pack x)
	case x1 of 
		Right x2 -> do 
			x <- Data.Vector.forM x2 
					(\x -> 
					case x of 
						Success y -> insertHistoricalPrice y)
			return $ Right x
		Left x2 -> do 
			liftIO $ Logger.errorM iModuleName ("Error Historical price "  `mappend` (show x))
			return $ Left x



insertOptionChainsIntoDb x y = do 
	Logger.debugM iModuleName ("Inserting " `mappend` show x `mappend` " " `mappend` show y)
	x1 <- getOptionChains x y 
	case x1 of 
		Right x2 -> do 
				liftIO $ Logger.debugM iModuleName ("Option chains " `mappend` (show x1))
				x <- Data.Vector.forM x2 (\s -> case s of 
						Success y -> insertOptionChain y
							) 
				return $ Right x
		Left x -> do
				liftIO $ Logger.errorM iModuleName ("Error processing option chain " `mappend` (show x))
				return $ Left x


setupSymbols aFileName = runResourceT $ 
			B.sourceFile aFileName $$ B.lines =$= parseSymbol =$= saveSymbol 
					=$= saveHistoricalData
					=$= saveOptionChains =$ consume


startup = do 
	dataDirectory <- getEnv("DATA_DIRECTORY")
	x <- return $ T.unpack $ T.intercalate "/" [(T.pack dataDirectory), "nasdaq_listed.txt"]
	y <- return $ T.unpack $ T.intercalate "/" [(T.pack dataDirectory), "other_listed.txt"]
	setupSymbols x 
	setupSymbols y 



startup_d = do 
	dataDirectory <- getEnv("DATA_DIRECTORY")
	x <- return $ T.unpack $ T.intercalate "/" [(T.pack dataDirectory), "nasdaq_10.txt"]
	setupSymbols x 


-- test query option chain 

testOptionChain aSymbol = queryOptionChain ("test"  :: String)
						$ toJSON $ QueryOptionChain "test" "Read" aSymbol []



