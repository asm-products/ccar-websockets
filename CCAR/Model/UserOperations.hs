module CCAR.Model.UserOperations (
	query
	, manage
	, UserOperations
)
where
import Control.Monad.IO.Class 
import Control.Monad.Logger 
import Control.Monad.Trans.Error 
import Control.Monad.Trans.State 
import Control.Monad.Trans.Maybe
import Control.Monad.Trans 
import Control.Applicative as Appl
import Database.Persist
import Database.Persist.Postgresql as Postgresql 
import Database.Persist.TH 
import CCAR.Main.DBUtils
import CCAR.Command.ApplicationError 
import CCAR.Model.Person

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


instance Manager UserOperations where 
	manage = manageUserOperations

data CRUD = Create | Read | C_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)


data UserOperations = UserOperations{operation :: CRUD, person :: Maybe Person} 
                deriving (Show, Eq)


instance ToJSON CRUD 
instance FromJSON CRUD

instance DBOps.ModuleInfo String where 
    name nick = "CCAR.Model.UserOperations->" ++ nick
instance DBOps.ModuleInfo T.Text where 
    name nick= "CCAR.Model.UserOperations -> " `mappend` (T.unpack nick)


genUserOperations  (UserOperations a b) = object [
    "commandType" .= ("ManageUser" :: String)
    , "operation" .= a 
    , "person" .= b] 

instance ToJSON UserOperations where
    toJSON = genUserOperations

parseUserOperations v = UserOperations <$> 
                v .: "operation" <*>
                (v .: "person")

instance FromJSON UserOperations where
    parseJSON (Object v) = parseUserOperations v
    parseJSON _          = Appl.empty


{-- 
processCommand (Just ( CommandUO (UserOperations uo aPerson))) = do
    infoM iModuleName $ show $ "Processing processCommand " ++ (show uo)
    person <- case aPerson of
                Just a -> return a        
    case uo of
        Us.Create  -> do
                personId <- insertPerson person
                Logger.infoM iModuleName $ show $ "Person inserted " ++ (show personId)
                return $ (GroupCommunication.Reply, CommandUO $ UserOperations Us.Create (Just person))
        Us.Update personId -> do
                updatePerson personId person
                return $ (GroupCommunication.Reply, CommandUO 
                            $ UserOperations (Us.Update personId) (Just person))
        Us.Delete personId -> do 
                deletePerson personId person
                return $ (GroupCommunication.Reply, CommandUO $ UserOperations (Us.Delete personId) (Just person))
        Us.Query personId -> do 
                maybePerson <- queryPerson (personId)
                case maybePerson of
                    Nothing -> 
                        return $ (GroupCommunication.Reply, CommandUO  $ UserOperations (Us.Query personId) Nothing)
                    Just (p) -> 
                        return $ (GroupCommunication.Reply, CommandUO  $ UserOperations (Us.Query personId) (Just p))
--}

iModuleName = "CCAR.Model.UserOperations"


--testUserOperations :: NickName -> Value -> Either String String 
testUserOperations a v = do 
	x <- Left $ "test"
	y <- case x of 
		Left x -> Left x 
		Right x -> Right x
	return y 

manageUserOperations :: NickName -> Value -> IO (GC.DestinationType, Either ApplicationError UserOperations)
manageUserOperations aNickName aValue@(Object a) = do
	Logger.debugM iModuleName $ show $ T.intercalate "-" ["inside manage", aNickName, 
				T.pack $ show aValue] 
	case (parse parseJSON aValue :: Result UserOperations) of 
		Success e@(UserOperations a b) -> do 			
			case a  of 
				Create -> createUO b 
				C_Update -> updateUO b  
				Read -> retrieveUO b 
				Delete -> deleteUO b 
		Error s -> return (GC.Reply, Left $ appError s)


createUO (Just aPerson) = do 
		personId <- insertPerson aPerson 
		Logger.infoM iModuleName $ "Person inserted " ++ (show personId)
		return (GC.Reply, Right $ UserOperations Create (Just aPerson))
updateUO (Just aPerson) = do 
		personId <- dbOps $ getBy $ UniqueNickName (personNickName aPerson)
{-		y <- runMaybeT $ lift $ dbOps $ getBy $ UniqueNickName (personNickName aPerson)
-}		x <- case personId of 
				Nothing -> return $ 
					Left $ appError ("Person not found " `mappend` (personNickName aPerson))
				Just (Entity pid per) -> do 
					up <- updatePerson pid aPerson 
					return $ Right $ (UserOperations C_Update up)
		return (GC.Reply, x)
retrieveUO (Just aPerson) = do 
	personId <- dbOps $ getBy $ UniqueNickName (personNickName aPerson) 
	x <- case personId of 
		Nothing -> return $ 
				Left $ appError ("Person not found " `mappend` (personNickName aPerson))
		Just(Entity pid per) -> do 
			ret <- queryPerson pid 
			return $ Right $ (UserOperations Read ret)
	return (GC.Reply, x)  


deleteUO (Just aPerson) = do 
	personId <- dbOps $ getBy $ UniqueNickName (personNickName aPerson) 
	x <- case personId of 
		Nothing -> return $ 
					Left $ appError ("Person not found " `mappend` (personNickName aPerson))
		Just (Entity pid per) -> do 
			ret <- deletePerson pid aPerson
			return $ Right $ (UserOperations Delete ret) 
	return (GC.Reply, x)
