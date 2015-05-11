module CCAR.Model.ProjectWorkbench(
	getSupportedScripts
)where 
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
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
import Data.Data
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
import qualified CCAR.Main.EnumeratedTypes as EnumeratedTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util as Util
import CCAR.Command.ErrorCommand
import Database.Persist.Postgresql as Postgresql 


data QuerySupportedScript = QuerySupportedScript {
		nickName :: T.Text
		, commandType :: T.Text
		, scriptTypes :: [EnumeratedTypes.SupportedScript]	
	} deriving (Show, Read, Data, Typeable, Generic)


instance ToJSON QuerySupportedScript
instance FromJSON QuerySupportedScript
getSupportedScripts :: T.Text -> Value -> IO(GC.DestinationType, T.Text)
getSupportedScripts n (Object a) = 
			return(GC.Reply, 
					Util.serialize $ QuerySupportedScript n 
					"GetSupportedScripts"
					EnumeratedTypes.getSupportedScripts)

