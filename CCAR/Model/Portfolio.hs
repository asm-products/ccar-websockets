module CCAR.Model.Portfolio (
	queryPortfolioSymbolTypes
	, queryPortfolioSymbolSides
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


data CRUD = Create | Read | P_Update | Delete 
	deriving(Show, Read, Eq, Data, Generic, Typeable)
	
instance ToJSON CRUD 
instance FromJSON CRUD 

data PortfolioCommands = PortfolioSymbolTypesQuery | PortfolioSymbolSidesQuery
		deriving (Show, Read, Data, Typeable, Generic)

data PortfolioSymbolTypeQuery = PortfolioSymbolTypeQuery {
		nickName :: T.Text
		, commandType :: T.Text
		, symbolTypes :: [EnTypes.PortfolioSymbolType]
} deriving(Show, Read, Data, Typeable, Generic)

{-- 
 Name mangling: https://mail.haskell.org/pipermail/haskell-cafe/2010-December/087862.html
--}
data PortfolioSymbolSideQuery = PortfolioSymbolSideQuery {
	pssNickName :: T.Text
	, pssCommandType :: T.Text
	, symbolSides :: [EnTypes.PortfolioSymbolSide]
} deriving(Show, Read, Data, Typeable, Generic)



instance ToJSON PortfolioCommands 
instance FromJSON PortfolioCommands
instance ToJSON PortfolioSymbolTypeQuery
instance FromJSON PortfolioSymbolTypeQuery 
instance ToJSON PortfolioSymbolSideQuery 
instance FromJSON PortfolioSymbolSideQuery

queryPortfolioSymbolTypes :: T.Text -> Value -> IO(GC.DestinationType, T.Text)
queryPortfolioSymbolTypes n (Object a) = 
			return(GC.Reply, 
					Util.serialize $ PortfolioSymbolTypeQuery n 
					(T.pack $ show PortfolioSymbolTypesQuery)
					EnTypes.getPortfolioSymbolTypes)
queryPortfolioSymbolSides :: T.Text -> Value -> IO (GC.DestinationType, T.Text)
queryPortfolioSymbolSides n (Object a) = 
		return (GC.Reply, 
				Util.serialize $ 
					PortfolioSymbolSideQuery n 
						(T.pack $ show PortfolioSymbolSidesQuery)
						EnTypes.getPortfolioSymbolSides)

type INickName = T.Text


data PortfolioT = PortfolioT {
	crudType :: CRUD
	, portfolioId :: UUID
	, companyId :: UUID
	, userId :: INickName
	, summary :: T.Text 
	, createdBy :: INickName
	, updatedBy :: INickName
}



dtoToDAo :: PortfolioT -> Portfolio 
dtoToDAo = undefined
daoToDto :: Portfolio -> PortfolioT 
daoToDto = undefined

process :: PortfolioT -> IO (Either T.Text (Key Portfolio))
process pT = case (crudType pT) of 
	Create -> insertPortfolio pT 
	Read -> readPortfolio pT -- single record
	P_Update -> updatePortfolio pT 
	Delete -> deletePortfolio pT 		


{--
        Portfolio json
            companyUserId CompanyUserId 
            uuid Text 
            summary Text -- A description about the portfolio
            createdBy PersonId 
            createdOn UTCTime default=CURRENT_TIMESTAMP
            updatedBy PersonId 
            updatedOn UTCTime default=CURRENT_TIMESTAMP
            UniquePortfolio uuid             
            deriving Show Eq
--}

insertPortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio) )
insertPortfolio p@(PortfolioT cType 
				_ 
				companyId 
				userId 
				summary 
				createdBy 
				_)= do 
	uuid <- nextUUID 
	case uuid of 
		Just u -> do 
			currentTime <- getCurrentTime
			dbOps $ do 
				c <- getBy $ CompanyUniqueID $ 
										uuidAsString companyId 
				nPerson <- getBy $ PersonUniqueNickName userId 
				crBy <- getBy $ PersonUniqueNickName createdBy
				case (c, nPerson, crBy) of 
					(Just (Entity cKey _)
						, Just (Entity nKey _)
						, Just (Entity crKey _) )-> do
						cuser <- getBy $ UniqueCompanyUser cKey nKey
						case cuser of 
							Just (Entity cuKey cuValue) -> do 
								porId <- insert $ Portfolio cuKey 
											(uuidAsString u)
											summary 
											crKey
											currentTime
											nKey 
											currentTime
								return $ Right porId
	 

uuidAsString uuid = T.pack $ UUID.toString uuid


updatePortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
updatePortfolio = undefined 

readPortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
readPortfolio = undefined

deletePortfolio :: PortfolioT -> IO (Either T.Text (Key Portfolio))
deletePortfolio = undefined

