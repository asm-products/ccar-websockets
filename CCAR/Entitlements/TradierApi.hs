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

authUrl =  "markets/quotes?symbols=spy"


makeUrl = S.unpackChars $ L.toStrict $ L.intercalate "/" [baseUrl, authUrl] 

getState = "abcd"
testTradier = do 
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
		liftIO $ print $ show req
		res <- http req manager
		return $ responseBody res 

