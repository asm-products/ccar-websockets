import Network.HTTP.Conduit 

-- The streaming interface
import System.Environment(getEnv)
import Data.Conduit 
import Data.Conduit.Binary (sinkFile)
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString.Internal as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Monoid()

baseUrl =  "https://sandbox.tradier.com/v1"

quotesUrl =  "markets/quotes"
timeAndSales = "markets/timesales"
optionChains = "markets/options/chains"


getMarketData url queryString fName= do 
	authBearerToken <- getEnv("TRADIER_BEARER_TOKEN") >>= 
		\x ->  return $ S.packChars $ "Bearer " ++ x
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
		responseBody res $$+- sinkFile fName
	where 
		makeUrl = S.unpackChars $ L.toStrict $ L.intercalate "/" [baseUrl, url]


getQuotes = \x y -> getMarketData quotesUrl [("symbols", Just x)] y

getTimeAndSales y = \x y-> getMarketData timeAndSales [("symbol", Just x)]

getOptionChains = \x y z-> do 
	getMarketData optionChains [("symbol", Just x), ("expiration", Just y )] z



