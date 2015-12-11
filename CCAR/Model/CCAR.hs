{--License: license.txt --}
module CCAR.Model.CCAR
(manage
 , parseCCMessage
 , CCARUpload
 , CCARText(..))
where
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql as Postgresql 
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ApplicationError 
import CCAR.Model.Person
import Database.Persist.Postgresql as DB

import Data.Text as T 
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import Data.Aeson
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Monoid
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L 

import GHC.Generics
import Data.Data
import Data.Typeable 
import Data.Time
import CCAR.Main.Util 
import System.Log.Logger as Logger
import CCAR.Main.DBOperations as DBOps
import CCAR.Parser.CCARParsec(readExpr)

data CRUD = Create  | C_Update | Query | Delete  | Read deriving(Show, Eq, Generic)
instance ToJSON CRUD
instance FromJSON CRUD


{- | Upload and retrieve ccar entries -}


iModuleName = "CCAR.Model.CCAR"
data CCARUpload = CCARUpload {uploadedBy :: T.Text 
                            , ccarOperation :: CRUD
                            , ccarData :: Maybe CCAR
                            , ccarResultSet :: [Maybe CCAR]} 
                    deriving (Show, Eq)

data CCARText = CCARText { textUploadedBy :: T.Text 
                            , scenarioName :: T.Text
                           , ccarText :: T.Text} deriving  (Show, Eq)

-- Clever code warning: the json parsing does the parse (need to fix this)
instance ToJSON CCARText where 
    toJSON (CCARText u s cc) = object ["textUploadedBy" .= u 
                                        , "scenarioName" .= s
                                        , "ccarText" .= (readExpr cc)
                                        , "commandType" .= ("ParsedCCARText" :: T.Text)]


genCCARUpload (CCARUpload a b c d ) = object["uploadedBy" .= a 
                                    , "ccarOperation" .= b 
                                    , "ccarData" .= c
                                    , "ccarResultSet" .= d
                                    , "commandType" .= (String "CCARUpload")]




instance ToJSON CCARUpload where
    toJSON = genCCARUpload 

parseCCARUpload v = CCARUpload <$>
                        v .: "uploadedBy" <*>
                        v .: "ccarOperation" <*>
                        v .: "ccarData" <*>
                        (pure [])

parseCCARText v = CCARText <$>
                    v .: "uploadedBy" <*>
                    v .: "scenarioName" <*>
                    v .: "ccarText"

parseCCAR v = CCAR <$>
                v .: "scenarioName" <*>
                v .: "scenarioText" <*>
                v .: "creator" <*>
                v .: "deleted"




instance FromJSON CCARUpload where 
    parseJSON (Object v) = parseCCARUpload v 
    parseJSON _          = Appl.empty


instance FromJSON CCARText where
    parseJSON (Object v) = parseCCARText  v 
    parseJSON _          = Appl.empty

instance Manager CCARUpload where
	manage = manageCCAR

manageCCAR :: NickName -> Value -> IO (GC.DestinationType, Either ApplicationError CCARUpload)
manageCCAR aNickName aValue = do 
	Logger.debugM iModuleName $ show $ T.intercalate "-" ["inside manage", aNickName, 
				T.pack $ show aValue] 
	case (AeTypes.parse parseJSON aValue) of 
		Success e@(CCARUpload a b c d) -> do 			
			case b  of 
				Create -> createCCAR e
				C_Update -> updateCCARU e   
				Read -> retrieveCCAR e 
				Delete -> deleteCCARU e  
		Error s -> return (GC.Reply, Left $ appError s)



parseCCMessage nickName aValue = do  
		case (AeTypes.parse parseJSON aValue :: Result CCARText) of
			Success r -> return (GC.Reply, Right r)
			Error s -> 
				return (
		    		GC.Reply
		    		, Left $ appError $ "Parse CCAR Text " `mappend` s)

insertCCAR :: CCAR -> IO (Key CCAR) 
insertCCAR c = dbOps $ do 
            cid <- DB.insert c
            $(logInfo) $ T.pack $ show ("Returning " ++ (show cid))
            return cid

updateCCAR :: CCAR -> IO (Maybe CCAR)
updateCCAR c = dbOps $ do
            DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARScenarioText =. (cCARScenarioText c)]
            return $ Just c 

queryAllCCAR :: T.Text -> IO [Entity CCAR]
queryAllCCAR aNickName = dbOps $ selectList [] []

type ScenarioName = T.Text 
queryCCAR :: ScenarioName -> IO (Maybe CCAR) 
queryCCAR scenarioName =  dbOps $ do 
                r <- getBy $ CCARUniqueName scenarioName 
                case r of 
                    Just (Entity k v) -> return $ Just v
                    Nothing -> return Nothing

deleteCCAR :: CCAR -> IO (Maybe CCAR)
deleteCCAR c = dbOps $ do
            DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARDeleted =. True]
            return $ Just $ c {cCARDeleted = True} 


createCCAR (CCARUpload a b (Just c) d) = do    
    ccarId <- insertCCAR c
    Logger.infoM iModuleName $ show $ "CCAR created " 
    return $ (GC.Reply, Right $ CCARUpload a b (Just c) [])

updateCCARU (CCARUpload a b (Just c) d) = do 
    c1 <- updateCCAR c 
    Logger.infoM iModuleName $ show "CCAR updated"
    return (GC.Reply, Right $ CCARUpload a b (Just c) [])
retrieveCCAR (CCARUpload a b (Just c) d) = do 
    c1 <- queryCCAR (cCARScenarioName c) 
    return (GC.Reply, Right $ CCARUpload a b c1 [])

deleteCCARU (CCARUpload a b (Just c) d) = do 
    c1 <- deleteCCAR c 
    return (GC.Reply, Right $ CCARUpload a b c1 [])