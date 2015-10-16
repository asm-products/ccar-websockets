module CCAR.Main.DBOperations 
	(query, Query, manage, Manager, ModuleInfo, name) where 
import Data.Text as T 
import Data.Aeson as J
import CCAR.Main.GroupCommunication as GC
import CCAR.Command.ApplicationError as AppError


class Query a where 
	query :: T.Text -> Value -> IO (GC.DestinationType, Either ApplicationError a) 


class Manager a where 
	manage :: T.Text -> Value -> IO (GC.DestinationType,  Either ApplicationError a)


class ModuleInfo a where 
	name  :: a -> String