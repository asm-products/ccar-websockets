module CCAR.Model.Login (
	query
    , Login(..)
    , LoginStatus(..)
)
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



instance DBOps.Query Login where 
    query = queryLogin  


data CRUD = Create | Read | C_Update | Delete
    deriving(Show, Eq, Read, Data, Generic, Typeable)

data LoginStatus = UserExists | UserNotFound | InvalidPassword | Undefined | Guest
    deriving(Show, Typeable, Data, Generic, Eq)


data Login  =    Login {login :: Maybe Person, loginStatus :: Maybe LoginStatus} 
                deriving (Show, Eq)



instance DBOps.ModuleInfo String where 
    name nick = "CCAR.Model.Login->" ++ nick
instance DBOps.ModuleInfo T.Text where 
    name nick= "CCAR.Model.Login -> " `mappend` (T.unpack nick)

{- handle login. Login returns a check for the user.-}
queryLogin nickName aValue = do 
    Logger.debugM (name nickName) $ show aValue
    x <- case (parse parseJSON aValue) of 
            Success login@(Login p status) -> do 
                chk <- checkLoginExists (nickName)
                case chk of 
                    Nothing -> return $ Right $ login {loginStatus = Just UserNotFound}
                    Just (Entity k val)       -> return $ Right $ login {login = Just val, 
                                                loginStatus = Just UserExists}
            Error s -> return $ Left $ appError s 
    return $ (GC.Reply, x)



instance ToJSON LoginStatus 
instance FromJSON LoginStatus

genLogin  (Login a b) = object [
    "commandType" .= (String "Login")
    , "login" .= Just a, "loginStatus" .= b]

instance ToJSON Login where
    toJSON = genLogin 

parseLogin v = Login <$> 
                v .: "login" <*>
                (v .: "loginStatus")

instance FromJSON Login where
    parseJSON (Object v) = parseLogin v
    parseJSON _          = Appl.empty
