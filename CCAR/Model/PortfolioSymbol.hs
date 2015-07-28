module CCAR.Model.PortfolioSymbol (
	manage
	) where 
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core

import Control.Monad.IO.Class(liftIO)
import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import Control.Exception
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
import GHC.IO.Exception

import Data.Data
import Data.Monoid (mappend)
import Data.Typeable 
import System.IO
import Data.Time
import Data.UUID.V1
import Data.UUID as UUID
import qualified CCAR.Main.EnumeratedTypes as EnTypes 
import qualified CCAR.Main.GroupCommunication as GC
import CCAR.Main.Util as Util
import CCAR.Command.ErrorCommand
import Database.Persist.Postgresql as Postgresql 
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))
import System.Log.Logger as Logger



iModuleName = "CCAR.Model.PortfolioSymbol"

data CRUD = Create | Read | UpdatePrime | Delete 
			deriving (Show, Read, Eq, Data, Generic, Typeable)
			

data PortfolioSymbolT = PortfolioSymbolT {
	commandType :: T.Text 
	, portfolioID :: T.Text -- unique uuid for the portfolio
	, symbol :: T.Text 
	, quantity :: Double 
	, side :: PortfolioSymbolSide 
	, symbolType :: PortfolioSymbolType 
	, createdBy :: T.Text 
	, updatedBy :: T.Text 
} deriving (Show, Read, Eq, Data, Generic, Typeable)

data PortfolioSymbolQueryT = PortfolioSymbolQueryT {
	qCommandType :: T.Text 
	, qPortfolioID :: T.Text
	, resultSet :: [PortfolioSymbolT]
} deriving (Show, Read, Eq, Data, Generic, Typeable)

queryPortfolioSymbol :: PortfolioSymbolQueryT -> IO (Either T.Text PortfolioSymbolQueryT) 
queryPortfolioSymbol = undefined 

dtoToDao :: PortfolioSymbolT -> IO PortfolioSymbol 
dtoToDao = undefined

daoToDto :: PortfolioSymbol -> IO (Either T.Text PortfolioSymbol) 
daoToDto = undfined 




-- create, read , update and delete operations
manage :: NickName -> Value -> IO (GC.DestinationType, T.Text)
manage = undefined

process :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol))
process = undefined 

insertPortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol))
insertPortfolioSymbol = undefined


updatePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol))
updatePortfolioSymbol = undefined

deletePortfolioSymbol :: PortfolioSymbolT -> IO (Either T.Text (Key PortfolioSymbol))
deletePortfolioSymbol = undefined




