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
import CCAR.Model.Portfolio as Portfolio 
import CCAR.Model.PortfolioSymbol as PortfolioSymbol
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))

class ModelHeader a where
	updateUUID ::  a -> IO a 

data TestModel = TestModel {uuid :: Maybe UUID
							, uuidAsString :: T.Text} deriving (Eq, Show)
instance ModelHeader TestModel where
	updateUUID p = do 
		uuid <- nextUUID
		case uuid of
			Just u -> do 
				uuidS <- return $ T.pack $ UUID.toString u
				return $ p{uuid = uuid, uuidAsString = uuidS}


{--| 
	To run the test cases, run config/dev.env
	$ config/dev.env
	$ cabal repl
	Call the tests manually. 
 |--}

{--| Test inserting a portfolio  |--}
testCase1 aCount = forM [1..aCount] $ return Portfolio.testInsertPortfolio

testCase2 aCount = do 
	portfolio <- Portfolio.testInsertPortfolio 
	case portfolio of 
		Right (portfolioID) -> do 
			forM [1..aCount] $ \x -> (PortfolioSymbol.testInsert x portfolioID)
		Left _ ->  return $ [Left $ "Failed inserting portfolio symbol"]


