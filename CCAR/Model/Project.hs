module CCAR.Model.Project
(
	manageProject
)

where
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
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import GHC.Generics
import Data.Data
import Data.Typeable 
import System.IO
import Data.Time

{-- 
 Create a project.
 Assign a set of images to a project
 assign a set of documents to project
--}

manageProject aNickName (Object a) = undefined