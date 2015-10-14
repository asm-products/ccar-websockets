module CCAR.Entitlements.Entitlements 
	(
		manage 
		, retrieve
		, query
		, testInsert
		, QueryEntitlementT
		, EntitlementT 
		, CompanyEntitlementT
		, QueryCompanyEntitlementT 
	)
where 

import CCAR.Main.DBUtils
import GHC.Generics
import Data.Aeson as J
import Yesod.Core

import Control.Applicative 
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
import CCAR.Command.ApplicationError
import Database.Persist.Postgresql as Postgresql 
-- For haskell shell
import HSH
import System.IO(openFile, writeFile, IOMode(..))
import System.Log.Logger as Logger
import CCAR.Main.DBOperations (Query, query, manage, Manager)

-- | Manage user entitlements.


newtype ModuleName = ModuleName {unMod :: T.Text} deriving (Show, Read, Eq, Data, Generic, Typeable)
type CommandType = T.Text
data CRUD = Create | Read | C_Update | Delete deriving (Show, Read, Eq, Data, Generic, Typeable)

instance FromJSON CRUD
instance ToJSON CRUD 


iModuleName = "CCAR.Model.Entitlements"
manageEntitlements = "ManageEntitlements"
manageCompanyEntitlements = "ManageCompanyEntitlements"



instance Manager EntitlementT where 
	manage = manageEntitlement 

instance Manager CompanyEntitlementT where 
	manage = manageCompanyEntitlement

instance Query QueryEntitlementT where 
	query = queryQe


instance Query QueryCompanyEntitlementT where 
	query = queryQCE



-- | Company-> User-> Entitlement uniquely identify this entitlement.
data CompanyEntitlementT = CompanyEntitlementT {
	entitlementCommandType :: CommandType 
	, entitlementCrudType :: CRUD 
	, companyID :: T.Text 
	, userId :: T.Text
	, entTabName :: T.Text 
	, entSecName :: T.Text
} deriving (Show, Read, Eq, Data, Generic, Typeable)


dtoCE coType crudType companyID userId tabName secName = 
		CompanyEntitlementT coType crudType companyID userId tabName secName


-- | Query company entitlements for a user
data QueryCompanyEntitlementT = QueryCompanyEntitlementT{
	qceEntitlementCommandType :: CommandType
	, qceNickName :: T.Text
	, qceCompanyName :: T.Text 
	, qceUserId :: T.Text
	, qceQueryParameters :: T.Text 
	, qceResultSet :: [CompanyEntitlementT]
} deriving (Show, Read, Eq, Data, Generic, Typeable)

instance ToJSON QueryCompanyEntitlementT where 
	toJSON ps@(QueryCompanyEntitlementT cType nn cName cUserId qP qR) = 
		object [
			"commandType" .= cType 
			, "nickName" .= nn 
			, "companyName" .= cName 
			, "userId" .= cUserId
			, "queryParameters" .= qP 
			, "resultSet" .= qR
		]

instance FromJSON QueryCompanyEntitlementT where 
	parseJSON (Object a) = QueryCompanyEntitlementT <$> 
		a .: "commandType" <*>
		a .: "nickName" <*> 
		a .: "companyName" <*> 
		a .: "userId" <*>
		a .: "queryParameters" <*>
		a .: "resultSet"
	parseJSON _ = Appl.empty


instance ToJSON CompanyEntitlementT where 
	toJSON cet@(CompanyEntitlementT eType crType companyID userId entTabName entSecName) = 
		object [
			"crudType" .= crType
			, "commandType" .= eType 
			, "companyId" .= companyID
			, "userId" .= userId
			, "tabName" .= entTabName
			, "sectionName" .= entSecName
		]



instance FromJSON CompanyEntitlementT where 
	parseJSON (Object a) = CompanyEntitlementT <$> 
			a .: "commandType" <*>
			a .: "crudType" <*> 
			a .: "companyId" <*> 
			a .: "userId" <*>
			a .: "tabName" <*> 
			a .: "sectionName"
	parseJSON _ = Appl.empty


data EntitlementT = EntitlementT {
	crudType :: CRUD 
	, commandType :: CommandType
	, tabName :: T.Text 
	, sectionName :: T.Text
	, entNickName :: T.Text} deriving (Show, Read, Eq, Data, Generic, Typeable)




instance ToJSON EntitlementT where 
	toJSON pS1@(EntitlementT crType coType tabName sectionName nn)= 
			object [
				"crudType" .= crType
				, "commandType" .= ( coType)
				, "tabName" .= tabName 
				, "sectionName" .= sectionName
				, "nickName" .=  nn
			]

instance FromJSON EntitlementT where 
	parseJSON (Object a ) = EntitlementT  <$> 
			a .: "crudType" <*>
			a .: "commandType" <*>
			a .: "tabName" <*> 
			a .: "sectionName" <*>
			a .: "nickName"
	parseJSON _ = Appl.empty


data QueryEntitlementT = QueryEntitlementT {
	qCommandType :: CommandType
	, qNickName :: T.Text 
	, queryParameters :: T.Text 
	, qResultSet :: [Entitlement]
} deriving(Show, Eq)

instance ToJSON QueryEntitlementT where 
	toJSON ps@(QueryEntitlementT cType nn qP qR) = 
		object [
			"commandType" .= cType 
			, "nickName" .= nn 
			, "queryParameters" .= qP 
			, "resultSet" .= qR
		]

instance FromJSON QueryEntitlementT where 
	parseJSON (Object a) = QueryEntitlementT <$> 
		a .: "commandType" <*>
		a .: "nickName" <*> 
		a .: "queryParameters" <*>
		a .: "resultSet"
	parseJSON _ = Appl.empty


-- | Assign entitlements for a user for a company
assign :: NickName -> Value  -> IO (GC.DestinationType, Either ApplicationError CompanyEntitlementT)
assign aNickName aValue = do 
	case (parse parseJSON aValue) of 
		Success ce@(CompanyEntitlementT coType crType cId userId tabName sectionName) -> 
				case crType of 
					Create -> createCompanyEntitlement ce 
					C_Update -> updateCompanyEntitlement ce 
					Read -> retrieveCompanyEntitlement ce 
					Delete -> deleteCompanyEntitlement ce



{- | NOTE: This will query all the entitlements that match a company id. -}
queryCompanyEntitlements comType cName userNickName = 
		dbOps $ do 
		company <- getBy $  UniqueCompanyId cName 
		userId <- getBy $ UniqueNickName userNickName 
		liftIO $ Logger.debugM iModuleName $ "User " ++ (show userId)
		resultSet <- case (company, userId) of 
						((Just (Entity cKey c)), (Just (Entity uKey u))) -> do 
							companyUserId <- getBy $ UniqueCompanyUser cKey uKey
							case companyUserId of 
								Just (Entity cuKey cuE) -> 
										selectList [CompanyUserEntitlementCompanyUserId ==. cuKey] 
														[Asc CompanyUserEntitlementCompanyUserId]
								Nothing -> selectList [] [Asc CompanyUserEntitlementCompanyUserId]
						(_, _) -> selectList [] [Asc CompanyUserEntitlementCompanyUserId]


		mapM (\a@(Entity k x) -> do 
			entitlement <- get $ companyUserEntitlementEntitlement x 
			case entitlement of 
				Just entitlement->  
						return $ CompanyEntitlementT comType Read userNickName cName 
									(entitlementSectionName entitlement) 
								 	(entitlementTabName entitlement)) resultSet 

{- | Return all the entitlements setup for this user for this company.
-}

queryQCE aNickName aValue@(Object a) = do 
	Logger.debugM iModuleName $ "Query company entitlements " 
	x <- case (parse parseJSON aValue) of 
		Success query@(QueryCompanyEntitlementT cType nn cName userId qP r) -> do 
			ents <- queryCompanyEntitlements cType cName userId 
			return $ Right $ QueryCompanyEntitlementT cType nn cName userId qP ents
	return (GC.Reply, x)

queryQe aNickName aValue@(Object a) = do 
	Logger.debugM iModuleName $ show aValue
	x <- case (parse parseJSON aValue) of 
			Success query@(QueryEntitlementT coType nickName param _) -> do 
				ents <- queryEntitlements 100
				entitlements <- mapM (\a@(Entity key val) -> return val) ents
				return $ Right $ QueryEntitlementT coType nickName param entitlements
			Error s -> return $ Left $ appError s 
	return (GC.Reply, x)

queryEntitlements :: Int -> IO [Entity Entitlement]
queryEntitlements limit = dbOps $ selectList [] [LimitTo limit]


{--  Manage : add/update/delete --}
manageEntitlement :: NickName -> Value -> IO (GC.DestinationType, Either ApplicationError EntitlementT)
manageEntitlement aNickName aValue@(Object a) = do
	Logger.debugM iModuleName $ show $ T.intercalate "-" ["inside manage", aNickName, 
				T.pack $ show aValue] 
	case (parse parseJSON aValue) of 
		Success e@(EntitlementT c co tab sec _) -> do 			
			case c of 
				Create -> create e 
				C_Update -> updateE e 
				Read -> retrieve e 
				Delete -> deleteE e
		Error s -> return (GC.Reply, Left $ appError s)


manageCompanyEntitlement :: NickName -> Value -> IO (GC.DestinationType, Either ApplicationError CompanyEntitlementT) 
manageCompanyEntitlement aNickName aValue = do 
	Logger.debugM iModuleName $ show $ T.intercalate "-" ["Inside manage", aNickName
										, T.pack $ show aValue]
	case (parse parseJSON aValue) of 
		Success ce -> do 
			case c of 
				Create -> createCompanyEntitlement ce 
				C_Update -> updateCompanyEntitlement ce 
				Read -> retrieveCompanyEntitlement ce 
				Delete -> deleteCompanyEntitlement ce 
			where 
				c = entitlementCrudType ce 
		Error s -> return (GC.Reply, Left $ appError s)


{-- Retrieve entitlements --}
retrieve e@(EntitlementT c co tab sec a) = do 
	Logger.debugM iModuleName $ show $ T.intercalate "-" ["Returning entitlements", tab, sec]
	dbOps $ do 
		chk <- getBy $ UniqueEntitlement tab sec 
		case chk of 
			Nothing -> return $ (GC.Reply, Left $ 
									appError $ 
										T.intercalate "->" 
											["Entitlement", tab, sec, "Not found"])
			Just (Entity k value) -> do 
					res <- return $ EntitlementT c co (entitlementTabName value) (entitlementSectionName value) a 
					return (GC.Reply, Right res)


create e@(EntitlementT  c co tab sec _) = do 
	dbOps $ do
		chk <- getBy $ UniqueEntitlement tab sec 

		case chk of 
			Nothing -> do 
					insert $ Entitlement tab sec 
					return (GC.Reply, Right e)
			Just (Entity p ent1) -> do 
					Postgresql.replace p $ Entitlement tab sec
					return (GC.Reply, Right e)



updateE e@(EntitlementT c co tab sec _) = do 
	dbOps $ do 
		chk <- getBy $ UniqueEntitlement tab sec 
		case chk of 
			Nothing -> return (GC.Reply, Left $ appError 
						$ T.intercalate "->" [tab, sec])
			Just (Entity pid entity) -> do 
				Postgresql.replace pid $ Entitlement tab sec 
				return (GC.Reply, Right e)

		

deleteE e@(EntitlementT c co tab sec _) = do 
	dbOps $ do 
		Postgresql.deleteBy $ UniqueEntitlement tab sec 
		return (GC.Reply, Right e)

{- | Create an entitlement for a user for a company.  -}
createCompanyEntitlement ce@(CompanyEntitlementT c crType companyId userId tab section) = do 
	dbOps $ do 
		chk <- getBy $ UniqueEntitlement tab section 
		company <- getBy $ UniqueCompanyId companyId
		user <- getBy $ UniqueNickName userId 
		case (chk, company, user)  of 
			(
				Just (Entity eid entitlement)
				, (Just (Entity cid cEntity))
				, (Just (Entity uid uEntity))
				) -> do 
					companyUser <- getBy $ UniqueCompanyUser cid uid
					case companyUser of 
						Nothing -> return (GC.Reply, Left $ appError $ T.intercalate "-" ["User not assigneg" 
																		, userId 
																		, companyId])
						Just (Entity cuId cu) -> do 
							insert $ CompanyUserEntitlement eid cuId
							return (GC.Reply, Right ce)
			_ -> return (GC.Reply, Left $ appError $ "Need to use traverse here" `mappend` (T.pack $ show ce))


{- | Updates an entitlement for a user for a company. Not implemented. We remove and add
     entitlements for a company for a user. -}
updateCompanyEntitlement ce@(CompanyEntitlementT c crType companyId userId tab section) = undefined

{- | Retrieves an entitlement for a user for a company. -}
retrieveCompanyEntitlement ce@(CompanyEntitlementT c crType companyId userId tab section) = do 
	dbOps $ do 
		ent <- getBy $ UniqueEntitlement tab section 
		company <- getBy $ UniqueCompanyId companyId 
		user <- getBy $ UniqueNickName userId
		case (ent, company, user) of 
			(	Just (Entity eid entitlement) 
				, Just (Entity cid cEntity)
				, Just (Entity uid uEntity) ) -> do 
				companyUser <- getBy $ UniqueCompanyUser cid uid 
				case companyUser of 
					Just (Entity cuId cu) -> do 
						cet <- getBy $ UniqueCompanyUserEntitlement eid cuId 
						case cet of 
							Just (Entity cueId cuEntitlement) -> do 
								return (GC.Reply, Right $ 
											CompanyEntitlementT c crType 
												companyId 
												userId 
												(entitlementTabName entitlement)
												(entitlementSectionName entitlement)
									)
							Nothing -> return (GC.Reply, Left $ appError $ "Record not found " `mappend` (T.pack $ show ce))
					Nothing -> return (GC.Reply, Left $ appError $ "Record not found " `mappend` (T.pack $ show ce))
			_ -> return (GC.Reply,  Left $ appError $ "Record not found " `mappend` (T.pack $ show ce))


{- | Delete an entitlement for a company for a user. -}
deleteCompanyEntitlement ce@(CompanyEntitlementT c crType companyId userId tab section) = do 
	dbOps $ do 
		ent <- getBy $ UniqueEntitlement tab section 
		company <- getBy $ UniqueCompanyId companyId 
		user <- getBy $ UniqueNickName userId 
		case (ent, company, user) of 
			(	Just (Entity eid entitlement) 
				, Just (Entity cid cEntity)
				, Just (Entity uid uEntity) ) -> do 
				companyUser <- getBy $ UniqueCompanyUser cid uid 
				case companyUser of 
					Just (Entity cuId cu) -> do 
						cet <- Postgresql.deleteBy $ UniqueCompanyUserEntitlement eid cuId
						return (GC.Reply, Right $ 
									CompanyEntitlementT c crType 
										companyId 
										userId 
										(entitlementTabName entitlement)
										(entitlementSectionName entitlement)
							)
					Nothing -> return (GC.Reply, Left $ appError $ "Record not found " `mappend` (T.pack $ show ce))
			_ -> return (GC.Reply,  Left $ appError $ "Record not found " `mappend` (T.pack $ show ce))




testInsert = do 
	dbOps $ do 
		chk <- getBy $ UniqueEntitlement tabName sectionName
		case chk of 
			Just (Entity p ent1) -> deleteBy $ UniqueEntitlement tabName sectionName
			Nothing -> liftIO $ putStrLn "Nothing to delete"

		e <- insert $ Entitlement tabName sectionName
		ent <- getBy $ UniqueEntitlement tabName sectionName
		(entR, j, k) <- case ent of 
			Just (Entity p ent1) -> do 				
					j@(Just jR) <- liftIO $ return $ Just $ toJSON ent1
					k <- liftIO $ return $ (fromJSON jR  :: Result Entitlement)
					case k of 
						Success l -> return (Just ent1, j, Just l)
			Nothing -> return (Nothing, Nothing, Nothing)
		return (entR, j, k)
	where
		tabName = "test1"
		sectionName = "test1"

testEntitlementT = do 
	x <- return $ EntitlementT Create ("ManageEntitlements") "t" "s" "n"
	y <- return $ toJSON x 
	z <- (fromJSON y :: Result EntitlementT)
	return (x, y, z)

testCommandType = do 
	x <- return $  "TestCommandType"
	y <- return $ toJSON x 
	z <- (fromJSON y :: Result CommandType)
	return (x, y)

testQueryEntitlements = do
	n <- return ("test" :: T.Text) 
	cType <- return ("QueryEntitlements" :: T.Text) 
	qParam <- return ("select where entitlement like something" :: T.Text)
	x <- return $ QueryEntitlementT cType n qParam []
	y <- return $ (toJSON x ::  Value)	
	query n y 