module CCAR.Entitlements.GmailAuthentication(getDefaultRequest) where
import Data.Text as T  hiding(foldl, foldr)
import Control.Applicative as Appl
import Data.Text.Encoding as E
import Data.Time
import Network.HTTP.Client as HttpClient
import Network.HTTP.Conduit 
import Data.Conduit 
import Data.Conduit.Binary(sinkFile)
import Network.HTTP.Types as W 
import Control.Monad.IO.Class(liftIO)
import Network.URI
import Data.Monoid(mappend)
import System.Environment(getEnv)
import Control.Monad.Trans.Resource(runResourceT)



data ResponseType = Token | ID_Token

instance Show ResponseType where 
	show Token = "Token"
	show ID_Token = "id_token"

data GmailOauth = GmailOauth {
			url :: URI
		, 	clientId :: T.Text
		,	redirectURI :: URI
		,   scope :: [URI]
		,   responseType :: [ResponseType]
	} deriving (Show)

getScope :: [URI] -> T.Text 
getScope a = foldr (mappend) " " (Prelude.map (T.pack . show) a)

getResponseType :: [ResponseType] -> T.Text 
getResponseType a = foldr mappend " "  (Prelude.map (T.pack . show ) a)

--createRequest :: GmailOauth -> IO Request 
createRequest g@(GmailOauth u c r s responseTypes) = do 
	req <- liftIO $ parseUrl (show u)
	queryString <- return  [("client_id", Just $ E.encodeUtf8 c )
						, ("redirect_uri", Just $ E.encodeUtf8 $ T.pack $ show r)
						, ("scope" , Just $ E.encodeUtf8 $ getScope s)
						, ("response_type", Just $ E.encodeUtf8 $ getResponseType responseTypes)
					] 
	nReq <- liftIO $ return $ setQueryString queryString req
	return nReq 

getDefaultRequest = do 
	req <- liftIO $ parseUrl "https://accounts.google.com/o/oauth2/auth"
	redirect <- liftIO $ parseUrl "http://chat.sarvabioremed.com/gmail_oauth2callback"
	s1 <- liftIO $ parseUrl "https://www.googleapis.com/auth/userinfo.email"
	s2 <- liftIO $  parseUrl "https://www.googleapis.com/auth/userinfo.profile"
	clientId <- liftIO $ getEnv "GMAIL_CLIENT_ID"
	reqU <- return . getUri $ req 
	redU <- return . getUri $ redirect 
	s1U <- return . getUri $ s1 
	s2U <- return . getUri $ s2
	g <- return $ GmailOauth reqU (T.pack clientId)
				redU ([s1U, s2U]) [Token, ID_Token]
	res <- createRequest g 
	return $ T.pack $ show $ getUri $ res


testRequest = 
	runResourceT $ do 
		manager <- liftIO $ newManager 
				$ tlsManagerSettings  {managerConnCount = 10}
		req <- liftIO $ parseUrl "https://chat.sarvabioremed.com"
		res <- http req manager 
		responseBody res $$+- sinkFile "foo.txt"


