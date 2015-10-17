{--License: license.txt --}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module CCAR.Main.Driver
    (driver)
where 

import Yesod.Core
import Yesod.WebSockets as YWS
import Control.Monad.Trans.Control    (MonadBaseControl (liftBaseWith, restoreM))
import Network.WebSockets.Connection as WSConn
import Network.WebSockets 
import Yesod.Static
import Control.Exception hiding(Handler)
import qualified GHC.Conc as GHCConc
import CCAR.Parser.CCARParsec
import Control.Monad (forever, void, when, liftM, filterM)
import Control.Monad.Trans.Reader
import Control.Monad.Error
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async as A (waitSTM, wait, async, cancel, waitEither, waitBoth, waitAny
                        , concurrently)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Data.Time
import Data.Monoid ((<>), mappend)
import Control.Concurrent.STM.Lifted
import Data.Text as T  hiding(foldl, foldr)
import Data.Aeson as J
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L hiding(foldl, foldr)
import System.IO
import Data.HashMap.Lazy as LH (HashMap, lookup, member)
import qualified CCAR.Model.Person as Us 
import qualified CCAR.Model.CCAR as CC 
import qualified CCAR.Model.UserTermsAndConditions as Ust 
import qualified CCAR.Model.Survey as Survey 
import Data.ByteString as DBS 
import Data.ByteString.Char8 as C8 
import System.Environment

import CCAR.Main.Util as Util
import GHC.Generics
import Data.Data
import Data.Typeable 
import Database.Persist.Postgresql as DB
import Database.Persist 
import Data.Map as IMap
import CCAR.Main.DBUtils
import CCAR.Main.GroupCommunication as GroupCommunication
import CCAR.Main.UserJoined as UserJoined 
import CCAR.Command.ApplicationError 
import CCAR.Model.Person
import CCAR.Model.Company as Company 
import CCAR.Model.Project as Project
import CCAR.Model.ProjectWorkbench as ProjectWorkbench
import CCAR.Model.Portfolio as Portfolio
import CCAR.Model.PortfolioSymbol as PortfolioSymbol
import CCAR.Entitlements.Entitlements as Entitlements
import CCAR.Model.Login as Login
import CCAR.Model.UserOperations as UserOperations
-- logging
import System.Log.Formatter as LogFormatter
import System.Log.Handler(setFormatter)
import System.Log.Handler.Simple as SimpleLogger
import System.Log.Handler.Syslog as SyslogLogger 
import System.Log.Logger as Logger
import System.Log as Log
import CCAR.Entitlements.GmailAuthentication as GmailAuthentication
import Network.URI
import Network.HTTP.Client as HttpClient
import Network.HTTP.Conduit 
import Network.HTTP.Types as W 




iModuleName :: String 
iModuleName = "CCAR.Main.Driver"


connStr = getConnectionString


data CheckPassword = CheckPassword {pwNickName :: T.Text, pwPassword :: T.Text, 
                passwordValid :: Maybe Bool, 
                numberOfAttemmpts :: Integer}
                    deriving(Show, Eq, Typeable, Data, Generic)

instance ToJSON CheckPassword 
instance FromJSON CheckPassword




data UserTermsOperations = UserTermsOperations {utOperation :: Ust.CRUD
                                        , terms :: Maybe TermsAndConditions} 
                                                deriving(Show, Eq)

-- There an absence of symmetry in this object: the result set can never be populated
-- as part of the request. Another clever thought.
data CCARUpload = CCARUpload {uploadedBy :: T.Text 
                            , ccarOperation :: CC.CRUD
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

type KeepAliveCommand = T.Text
type From = T.Text
type To = T.Text


-- Chose not to name the Message component as Message and left as T.Text ??
-- Will bite me.
data Command = CommandUTO UserTermsOperations
                | CommandCCARUpload CCARUpload
                | CommandError ApplicationError 
                | CommandKeepAlive KeepAliveCommand
                | ParseCCARText CCARText
                deriving(Show, Eq)

data UserPreferences = UserPreferences {prefs :: T.Text} deriving (Show, Eq, Generic)


genPerson (Person a b c d e f) = object ["firstName" .= a
                                       , "lastName" .= b
                                       , "nickName" .= c
                                       , "password" .= d
                                       , "locale" .= e
                                       , "lastLoginTime" .= f]

genCCAR (CCAR a b person del)  = object ["scenarioName" .= a 
                                    , "scenarioText" .= b
                                    , "creator" .= person
                                    , "deleted" .= del]

genCCARUpload (CCARUpload a b c d ) = object["uploadedBy" .= a 
                                    , "ccarOperation" .= b 
                                    , "ccarData" .= c
                                    , "ccarResultSet" .= d
                                    , "commandType" .= (String "CCARUpload")]

genUserTermsOperations (UserTermsOperations o t) = object ["utOperation" .= o, "terms" .= t]

genTermsAndConditions (TermsAndConditions t des accept) = object ["title" .= t
                                            , "description" .= des
                                            , "acceptDate" .= accept]
genCommandKeepAlive a  = object ["KeepAlive" .= a
                                , "commandType" .= ("KeepAlive" :: T.Text)]



instance ToJSON UserTermsOperations where
    toJSON = genUserTermsOperations


instance ToJSON CCARUpload where
    toJSON = genCCARUpload 

instance ToJSON CCAR where
    toJSON = genCCAR 

instance ToJSON Command where
    toJSON aCommand = 
        case aCommand of
            CommandError e -> toJSON e
            CommandCCARUpload a  -> genCCARUpload a
            CommandKeepAlive a -> genCommandKeepAlive a
            ParseCCARText a -> toJSON a
            _ -> toJSON $ ApplicationError {errorCode = "Unknown" :: T.Text , 
                        message = T.pack (show aCommand)}

commandType :: HashMap T.Text Value -> Maybe Value
commandType = LH.lookup "commandType"

parseCommand value = do
        case (commandType value) of
            Nothing -> CommandError <$> (pure $ appError value)
            Just cType -> 
                case (cType) of 
                    "CCARUpload" -> CommandCCARUpload <$> parseCCARUpload value
                    "KeepAlive" -> CommandKeepAlive <$> parseKeepAlive value
                    "ParsedCCARText" -> ParseCCARText <$> parseCCARText value
                    _       -> CommandError <$>  (pure $ appError value)


parseKeepAlive v = v .: "keepAlive"

-- The upload 
parseCCARUpload v = CCARUpload <$>
                        v .: "uploadedBy" <*>
                        v .: "ccarOperation" <*>
                        v .: "ccarData" <*>
                        (pure [])

parseCCAR v = CCAR <$>
                v .: "scenarioName" <*>
                v .: "scenarioText" <*>
                v .: "creator" <*>
                v .: "deleted"

parsePerson v = do 
                firstName <- v .: "firstName"
                lastName <- v .: "lastName"
                nickName <- v .: "nickName"
                password <- v .: "password"
                deleted <- v .: "deleted"
                time <- v .: "lastLoginTime"
                return $ Person firstName lastName nickName password time deleted



parseTermsAndConditions v = TermsAndConditions <$>
                        v .: "title" <*>
                        v .: "description" <*>
                        v .: "acceptDate"

parseCCARText v = CCARText <$>
                    v .: "uploadedBy" <*>
                    v .: "scenarioName" <*>
                    v .: "ccarText"


instance FromJSON CCAR where 
    parseJSON (Object v) = parseCCAR v
    parseJSON _          = Appl.empty

instance FromJSON Command where
    parseJSON (Object v) = parseCommand v
    parseJSON _         = Appl.empty

instance FromJSON CCARText where
    parseJSON (Object v) = parseCCARText  v 
    parseJSON _          = Appl.empty


instance FromJSON CCARUpload where 
    parseJSON (Object v) = parseCCARUpload v 
    parseJSON _          = Appl.empty


iParseJSON :: (FromJSON a) => T.Text -> Either String (Maybe a)
iParseJSON = J.eitherDecode . E.encodeUtf8 . L.fromStrict

pJSON :: (FromJSON a) => T.Text -> IO (Either String (Maybe a))
pJSON  aText = do
    Logger.infoM  iModuleName ( T.unpack aText)
    return $ iParseJSON aText

decoder :: Command -> T.Text 
decoder = L.toStrict . E.decodeUtf8 . En.encode        
decoderM a = return $ L.toStrict $ E.decodeUtf8 $ En.encode a



type NickName = T.Text
type ClientMap = GroupCommunication.ClientIdentifierMap

-- the broadcast channel for the application.
data App = App { chan :: (TChan T.Text)
                , nickNameMap :: ClientMap}

instance Yesod App

mkYesod "App" [parseRoutes|
/chat HomeR GET
|]


postGmailOauthR :: T.Text -> T.Text -> Handler T.Text
postGmailOauthR = undefined
getGmailOauthR :: T.Text -> T.Text -> Handler T.Text 
getGmailOauthR a b = return $ a `mappend` b


checkPassword :: CheckPassword -> IO (DestinationType, CheckPassword) 
checkPassword b@(CheckPassword personNickName password _ attempts) = do
    chk <- checkLoginExists(personNickName) 
    case chk of 
        Nothing -> return $ (GroupCommunication.Reply, b {passwordValid = (Just False)})
        Just (Entity aid a) -> do
            return $ (Reply, b {passwordValid = validatePassword (personPassword a) b})

validatePassword :: T.Text -> CheckPassword -> Maybe Bool 
validatePassword dbPassword input = Just $ dbPassword == (pwPassword input)


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




processCommand :: Maybe Command  -> IO (DestinationType, Command)
processCommand (Just (CommandError error)) = 
        return $ (GroupCommunication.Reply, CommandError error)

processCommand Nothing = return $  
                        (GroupCommunication.Reply, CommandError $ 
                            appError ("Unable to process command" :: T.Text))

processCommand (Just (CommandKeepAlive a)) = 
        return $ (GroupCommunication.Reply, CommandKeepAlive a)


-- | Query operations are replies and db operations are broadcast. | --
processCommand (Just (CommandCCARUpload (CCARUpload nickName operation aCCAR aList))) = do
    Logger.infoM iModuleName $ show $ "Processing command ccar upload " ++ (show ccar)
    case operation of 
        CC.Create -> do 
                ccarId <- insertCCAR ccar
                Logger.infoM iModuleName $ show $ "CCAR created " ++ (show ccar)
                return $ (Broadcast, CommandCCARUpload $ CCARUpload nickName operation 
                                (Just ccar) [])
        CC.Update  -> do
                updateCCAR ccar 
                return $ (Broadcast, CommandCCARUpload $ CCARUpload nickName operation (Just ccar) [])
        CC.Delete  -> do 
                res <- deleteCCAR ccar 
                return $ (Broadcast, CommandCCARUpload $ CCARUpload nickName operation (res) [])
        CC.Query -> do 
                case aCCAR of 
                    Just x -> do 
                        maybeCCAR <- queryCCAR (cCARScenarioName x)
                        case maybeCCAR of 
                            Nothing -> return $ (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName CC.Query Nothing [])
                            Just x -> return $  (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName CC.Query (Just x) []) 
        CC.QueryAll nickName -> do
                maybeCCAR <- queryAllCCAR nickName
                case maybeCCAR of 
                    [] -> return $ (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName (CC.QueryAll nickName) Nothing [])
                    aList -> return $ 
                            (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName (CC.QueryAll nickName) Nothing (ccarList aList))
                    where
                        ccarList aList = Prelude.map( \(Entity y  x) -> Just x) aList
        where
            ccar = case aCCAR of
                    Just a -> a


processCommand (Just a) = return (GroupCommunication.Reply, a)

processCommandValue :: App -> T.Text -> Value -> IO (DestinationType, T.Text)
processCommandValue app nickName aValue@(Object a)   = do  
    case cType of 
        Nothing -> return $ (GroupCommunication.Reply, ser $ 
                        CommandError $ appError ("Unable to process command" :: T.Text))
        Just aType -> 
            case aType of 
                String "UserBanned" -> do
                        c <- return $ (parse parseJSON aValue :: Result UserBanned)
                        case c of
                            Success u@(UserJoined.UserBanned a1) -> do
                                bConns <- atomically $ getClientState a1 app 
                                mapM_ (\bconn -> WSConn.sendClose (connection bconn)
                                        ("Bye"
                                            :: T.Text) `catch` (
                                            \c@(ConnectionClosed) -> atomically $ deleteConnection app a1                    
                                            )) bConns  -- To handle multiple connections to a client.
                                return (GroupCommunication.Broadcast, ser u)
                            Error s ->  return (GroupCommunication.Reply 
                                    , ser $ CommandError $ appError $ "parse manage user failed " ++ s )

                String "CCARUpload" ->
                        case (parse parseJSON aValue :: Result CCARUpload) of
                            Success r -> do  
                                    (d, c) <- processCommand $ Just $ CommandCCARUpload r 
                                    return (d, ser c)
                            Error s -> 
                                return (GroupCommunication.Reply 
                                    , ser $ 
                                        CommandError $ appError $ "parse ccar upload failed " ++ s ++ (show a))
                String "KeepAlive" ->
                        case (parse parseKeepAlive a) of
                            Success r -> do 
                                    (d, c) <- processCommand $ Just $ CommandKeepAlive r 
                                    return (d, ser c)
                            Error s -> 
                                return (
                                    GroupCommunication.Reply, 
                                    ser $ CommandError $ appError $ 
                                        "Parse Keep alive failed" ++ s ++ (show a))
                String "ParsedCCARText" ->
                        case (parse parseJSON aValue :: Result CCARText) of
                            Success r -> do 
                                    (d, c) <- processCommand $ Just $ ParseCCARText r 
                                    return (d, ser c)
                            Error s -> 
                                return (
                                    GroupCommunication.Reply
                                    , ser $ CommandError $ appError $ "Parse CCAR Text " ++ s ++ (show a)
                                )
                String "SendMessage" -> do 
                        (dType, value) <- processSendMessage (Object a)
                        return (dType, ser value)
                String "ManageCompany" -> Company.manageCompany nickName (Object a)
                String "SelectAllCompanies" -> Company.queryAllCompanies nickName (Object a)
                String "ManageProject" -> Project.manageProject nickName (Object a)
                String "SelectActiveProjects" -> Project.queryActiveProjects nickName (Object a)
                String "QuerySupportedScripts" -> ProjectWorkbench.querySupportedScripts nickName (Object a)
                String "QueryActiveWorkbenches" -> ProjectWorkbench.queryActiveWorkbenches (Object a)
                String "PortfolioSymbolTypesQuery" -> Portfolio.queryPortfolioSymbolTypes nickName (Object a)
                String "PortfolioSymbolSidesQuery" ->Portfolio.queryPortfolioSymbolSides nickName (Object a)
                String "ManageWorkbench" -> ProjectWorkbench.manageWorkbench (Object a)
                String "ExecuteWorkbench" -> do                            
                            atomically $ do 
                                clientStates <- getClientState nickName app 
                                mapM_ (\cs -> writeTChan (jobWriteChan cs) (Object a)) clientStates                            
                            return(GroupCommunication.Reply, 
                                ser $ ("Execute workbench received" :: T.Text))
                --ProjectWorkbench.executeWorkbench(Object a)
                String "ManageSurvey" -> Survey.manageSurvey nickName (Object a)
                -- Assign a user to a company.
                String "AssignCompany" -> Company.assignUserToCompany nickName (Object a)
                String "QueryPortfolios" -> Portfolio.manageSearch nickName (Object a)
                String "ManagePortfolio" -> Portfolio.manage nickName (Object a)
                String "ManagePortfolioSymbol" -> PortfolioSymbol.manage nickName (Object a)
                String "QueryPortfolioSymbol" -> PortfolioSymbol.manageSearch nickName (Object a)
                String "ManageEntitlements" -> Entitlements.manage nickName aValue 
                                                            >>= \(gc, either) -> 
                                                                return (gc, Util.serialize 
                                                                    (either :: Either ApplicationError Entitlements.EntitlementT)
                                                                    )
                String "QueryEntitlements" -> Entitlements.query nickName aValue 
                                                            >>= \(gc, either) -> 
                                                                return (gc, 
                                                                    Util.serialize 
                                                                    (either :: 
                                                                            Either ApplicationError Entitlements.QueryEntitlementT))
                String "ManageCompanyEntitlements" -> Entitlements.manage nickName aValue 
                                                        >>= \(gc, either) ->
                                                            return (gc , 
                                                                Util.serialize 
                                                                    (either :: Either ApplicationError 
                                                                                Entitlements.CompanyEntitlementT))
                String "QueryCompanyEntitlements" -> Entitlements.query nickName aValue 
                                                    >>= \(gc, either) ->
                                                        return (gc, Util.serialize
                                                                (either :: 
                                                                    Either ApplicationError Entitlements.QueryCompanyEntitlementT))
                String "Login" -> 
                            Login.query nickName aValue 
                                >>= \(gc, result) -> 
                                    return (gc, 
                                            Util.serialize
                                            (result ::Either ApplicationError Login))
                String "ManageUser" ->
                        UserOperations.manage nickName aValue 
                            >>= \(gc, either) ->
                                return(gc, Util.serialize 
                                        (either :: Either ApplicationError UserOperations))

                _ ->                                                
                    return 
                         ( GroupCommunication.Reply
                         , ser $ CommandError $ appError ("Unable to process command " ++ (show aType)))
    where 
        cType =  LH.lookup "commandType" a
        ser a = L.toStrict $ E.decodeUtf8 $ En.encode a 


lookupTag :: Maybe Value -> T.Text -> IO (Maybe Value)
lookupTag aCommand aTag = do
    case aCommand of 
        Just (Object a) -> return $ LH.lookup aTag  a 

getNickName :: Maybe Value -> IO (Maybe T.Text, T.Text)
getNickName aCommand = 
    do
        case aCommand of
            Nothing -> return $ 
                (Nothing, L.toStrict $ E.decodeUtf8 $ En.encode $ 
                    CommandError $ appError ("Unknown error" :: T.Text))
            Just (Object a) -> 
                  case nn of
                    Nothing -> return (Nothing, "Nickname tag not found")
                    Just x -> 
                        case x of
                            String x -> return (Just "found nickName", x)
                            _ -> return (Nothing, T.pack $ "Invalid " ++ (show x))
                where 
                    nn = LH.lookup "nickName" a 

processIncomingMessage :: App -> WSConn.Connection -> T.Text ->  Maybe Value -> IO (DestinationType , T.Text)
processIncomingMessage app conn aNickName aCommand = do 
    case aCommand of 
        Nothing -> do
                Logger.infoM iModuleName $ "Processing error..."
                result <- return (CommandError $ appError ("Unknown error" :: T.Text)) 
                return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode  result)
                    
        Just (Object a) -> do 
                Logger.infoM iModuleName $ show $ "Processing command type " ++ (show (LH.lookup "commandType" a))
                (processCommandValue app aNickName (Object a)) `catch`
                    (\e -> do
                            Logger.errorM iModuleName $  ("Exception "  ++ show (e :: PersistException)) 
                            atomically $ deleteConnection app aNickName
                            return (GroupCommunication.Broadcast, 
                                    Util.serialize $ UserJoined.userLeft aNickName)
                            )
                --return $ (d, L.toStrict $ E.decodeUtf8 $ En.encode command)
                    


deleteConnection :: App -> T.Text -> STM  () 
deleteConnection app nn = do 
            cMap <- readTVar $ nickNameMap app                
            _ <-    writeTVar (nickNameMap app) (IMap.delete nn cMap)
            return ()

addConnection :: App -> WSConn.Connection ->  T.Text -> STM ()
addConnection app aConn nn = do 
                nMap <- readTVar $ nickNameMap app 
                clientState <- GroupCommunication.createClientState nn aConn
                _ <- writeTVar (nickNameMap app) (IMap.insert nn clientState nMap)
                return ()


getAllClients :: App -> T.Text -> STM [ClientState]
getAllClients app@(App a c) nn = do
    nMap <- readTVar c 
    return $ Prelude.filter (\x -> nn /= (nickName x)) $ elems nMap 
getClientState :: T.Text -> App -> STM [ClientState]
getClientState nickName app@(App a c) = do
        nMap <- readTVar c
        if IMap.member nickName nMap then 
            return $ [nMap ! nickName]
        else 
            return [] 

getPersonNickName :: Maybe Person -> IO T.Text
getPersonNickName a = do
    case a of 
        Just x -> return $  personNickName x
        Nothing -> return "Invalid nick name"

authenticate :: WSConn.Connection -> T.Text -> App -> IO (DestinationType, T.Text)
authenticate aConn aText app@(App a c) = 
    do 
        case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                        L.toStrict $ E.decodeUtf8 $ En.encode $ CommandError 
                                $ appError ("Invalid command during login" :: T.Text))
            Just o@(Object a) -> do                
                result <- return $ (parse parseJSON o :: Result Login)
                case result of 
                    Success (r@(Login a b)) -> do 
                            nickName <- getPersonNickName a 
                            userJoined <- return $ UserJoined.userJoined nickName 
                            return (GroupCommunication.Reply, userJoined)
                    Error s -> 
                        return (GroupCommunication.Reply, T.pack s)
        where 
            aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value




processUserLoggedIn :: WSConn.Connection -> T.Text -> App -> IO (DestinationType, T.Text) 
processUserLoggedIn aConn aText app@(App a c) = do
    case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                    ser $ CommandError $ appError ("Login has errors" :: T.Text))
            Just o@(Object a) -> do
                Just commandType <- return $ LH.lookup "commandType" a
                case commandType of 
                    String "UserLoggedIn" -> do 
                        c <- return $ (parse parseJSON o :: Result UserLoggedIn)
                        case c of 
                            Success u@(UserJoined.UserLoggedIn a) -> do 
                                        atomically $ addConnection app aConn a 
                                        return $ (Broadcast, ser u)
                            _ -> return $ (GroupCommunication.Reply, ser  
                                                $ CommandError $ 
                                                appError ("Invalid command during login" :: T.Text))                                
                    String "UserJoined" -> do 
                        c <- return $ (parse parseJSON o :: Result UserJoined)
                        case c of 
                            Success u@(UserJoined.UserJoined a) -> do 
                                        return $ (Broadcast, ser u)
                            _ -> return $ (GroupCommunication.Reply, ser  
                                                $ CommandError $ 
                                                appError 
                                                    ("Invalid command during login" :: T.Text))
                    String "ManageUser"-> do 
                        g@(gc, res) <- UserOperations.manage aText o 
                        case res of 
                            Right x -> atomically $ addConnection app aConn aText            
                        return (gc, ser (res :: Either ApplicationError UserOperations )) 
                    String "GuestUser" -> do 
                        result <- return $ (parse parseGuestUser a)
                        case result of 
                            Success (g@(UserJoined.GuestUser guestNickName)) -> do
                                    createGuestLogin guestNickName  
                                    atomically $ addConnection app aConn guestNickName
                                    return $ (GroupCommunication.Broadcast, ser g)                          
                            Error errMessage ->  
                                    return (GroupCommunication.Reply 
                                        , ser $ CommandError $ appError
                                                $ ("Guest login failed ") `mappend`  errMessage )
                    _ -> return (GroupCommunication.Reply 
                                , ser $ CommandError $ appError 
                                        $ "process user logged in failed :  " ++ (show aText) )

            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value
                ser  = (L.toStrict) . (E.decodeUtf8) . (En.encode)

instance Show WSConn.Connection where
    show (WSConn.Connection o cType proto msgIn msgOut cl) = show proto 

-- Do not let multiple connections to the same nick name.
-- How do we allow multiple connections for handling larger data, such as
-- video or file upload?
-- Create a session id for a connection and send that token back to the client.
-- Subsequent request for another connection needs to be assigned the same token. 


ccarApp :: WebSocketsT Handler ()
ccarApp = do
        connection <- ask
        app <- getYesod
        liftIO $ Logger.debugM iModuleName "Before receiving data..."
        command <- liftIO $ WSConn.receiveData connection
        (result, nickNameV) <- liftIO $ getNickName $ incomingDictionary (command :: T.Text)
        clientState <- atomically $ getClientState nickNameV app
        liftIO $  Logger.debugM iModuleName "Before showing client state"
        liftIO $ Logger.debugM iModuleName $ show clientState 
        case clientState of 
            [] -> do 
                (destination, text) <- liftIO $ authenticate connection command app  
                $(logInfo) $ "Incoming text " `mappend` (command :: T.Text)
                $(logInfo) $ T.pack $ show $ incomingDictionary (command :: T.Text)
                processResult <- case result of 
                    Nothing -> do 
                            $(logInfo) command 
                            liftIO $ do  
                                        _ <- WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                                        return "Close sent"
                    Just _ -> liftIO $ do 
                            (processClientLost app connection nickNameV command)
                             `catch` 
                                    (\ a@(CloseRequest e1 e2) -> do  
                                        atomically $ deleteConnection app nickNameV
                                        return "Close request" )
                            a <- liftBaseWith (\run -> run $ liftIO $ do
                                            a <- (A.async (writerThread app connection 
                                                nickNameV False))
                                            b <- (A.async (liftIO $ readerThread app nickNameV False))
                                            c <- (A.async $ liftIO $ jobReaderThread app nickNameV False)
                                            A.waitAny [a,  b,  c]
                                            return "Threads had exception") 
                            return ("All threads exited" :: T.Text)
                return () 
            _ -> liftIO $ WSConn.sendClose connection ("Active connection. Multiple logins not allowed. " `mappend` nickNameV)



incomingDictionary aText = J.decode  $ E.encodeUtf8 $ L.fromStrict aText :: Maybe Value


{-- Both these methods are part of pre-login handshake. --}
{-- An exception while server is replying to client. --}
processClientLost app connection nickNameV iText = do
                    (command, nickNameFound) <- liftIO $ processIncomingMessage 
                                app 
                                connection 
                                nickNameV
                                $ incomingDictionary iText
                    liftIO $ Logger.errorM iModuleName $ "Sending " ++ ( show nickNameFound)
                    WSConn.sendTextData connection nickNameFound
                    (processClientLeft connection app nickNameV) `catch`
                                    (\ a@(CloseRequest e1 e2) -> do  
                                        atomically $ deleteConnection app nickNameV
                                        return "Close request" )
                    return ("Threads exited" :: T.Text)

{- Stay inside the loop till the user answers with the correct passsword -}
processUserPassword connection app nickNameV = undefined

{-- Client hits a refresh or loses connection --}
processClientLeft connection app nickNameV = do
            command <- WSConn.receiveData connection
            (dest, text) <- liftIO $ processUserLoggedIn connection command app 
            Logger.debugM iModuleName $ "User logged in " ++ (show text)
            messageLimit <- liftIO $ getMessageCount nickNameV
            Logger.debugM iModuleName  $ "Using message limit " ++ (show messageLimit)
            messageHistory <- liftIO $ GroupCommunication.getMessageHistory messageLimit
            Logger.debugM iModuleName $ "After messageHistory " ++ (show nickNameV)
            atomically $ do 
                            clientStates <- case dest of 
                                Broadcast -> getAllClients app nickNameV
                                PrivateMessage t ->
                                    getClientState t app
                                _ ->
                                    getClientState nickNameV app           
                            --                              
                            mapM_ (\cs -> writeTChan (writeChan cs) (text)) clientStates                                        
                            clientStates <- getClientState nickNameV app 
                            mapM_ (\cs -> writeTChan (writeChan cs) 
                                    (UserJoined.userLoggedIn (nickName cs))) clientStates
                            currentClientState <- getClientState nickNameV app
                            allClients <- getAllClients app nickNameV
                            mapM_ (\conn -> 
                                    mapM_ (\cs -> 
                                            writeTChan (writeChan conn) 
                                                (UserJoined.userLoggedIn (nickName cs)) 
                                            ) allClients
                                    ) currentClientState
                            
                            mapM_ (\text -> 
                                        mapM_ (\cs -> 
                                                    writeTChan (writeChan cs) text) 
                                                    currentClientState
                                        ) messageHistory
            return "Threads Exiting"

handleDisconnects :: App -> WSConn.Connection -> T.Text -> ConnectionException -> IO ()
handleDisconnects app connection nickN (CloseRequest a b) = do 
            Logger.errorM iModuleName $ T.unpack $  
                ("Bye nickName " :: T.Text) `mappend` nickN
            atomically $ do 
                deleteConnection app nickN
                restOfUs <- getAllClients app nickN
                case restOfUs of 
                    x  : _ -> do  
                        mapM_ (\cs -> 
                                writeTChan(writeChan cs) $ 
                                    UserJoined.userLeft nickN )restOfUs
                    [] -> return ()
            
handleDisconnects app connecction nickN c = do 
    Logger.errorM iModuleName $ T.unpack $ 
                    ("Bye nickname " :: T.Text) `mappend` nickN 
    atomically $ do 
        deleteConnection app nickN 
        restOfUs <- getAllClients app nickN
        case restOfUs of 
            x  : _ -> do  
                mapM_ (\cs -> 
                        writeTChan(writeChan cs) $ 
                            UserJoined.userLeft nickN )restOfUs
            [] -> return ()

readerThread :: App -> T.Text -> Bool -> IO ()
readerThread app nickN terminate = do
    if (terminate == True) 
        then do 
            Logger.infoM iModuleName "Reader thread exiting" 
            return () 
    else do 
        Logger.infoM iModuleName "Waiting for messages..."
        (conn , textData) <- atomically $ do
                clientStates <- getClientState nickN app 
                case clientStates  of 
                    clientState : _ -> do 
                        textData <- readTChan (readChan clientState)                        
                        return (Just $ connection clientState, textData)
                    [] -> return (Nothing, "")
        x <- case conn of 
                Just connection -> do 
                                _ <- WSConn.sendTextData (connection) textData `catch` 
                                        (\h@(CloseRequest e f)-> do 
                                                    handleDisconnects app 
                                                        connection nickN h
                                                    readerThread app nickN True)
                                liftIO $ Logger.debugM iModuleName 
                                            $ "Wrote " `mappend` 
                                            (show $ T.take 150 textData) `mappend` (show conn)
                                readerThread app nickN terminate
                Nothing -> readerThread app nickN True  
        return x
jobReaderThread :: App -> T.Text -> Bool -> IO ()
jobReaderThread app nickN terminate = 
    if(terminate == True) then do 
        Logger.infoM iModuleName "Job reader thread exiting."
        return ()
    else do
        Logger.infoM iModuleName "Waiting for jobs..."
        (conn , value) <- atomically $ do
                clientStates <- getClientState nickN app 
                case clientStates  of 
                    clientState : _ -> do 
                        textData <- readTChan (jobReadChan clientState)
                        return (Just $ connection clientState, textData)
                    [] -> return (Nothing, "Client state doesnt exist")
        Logger.infoM iModuleName ("Reading a job " ++ (show value))
        case conn of 
            Just connection -> do
                        (replyType, text) <- 
                            (ProjectWorkbench.executeWorkbench value)
                                `catch` (\ x@(SomeException e) -> return (Reply, "Exception in job " :: T.Text))
                        _ <- WSConn.sendTextData (connection) text `catch` 
                                (\h@(CloseRequest e f)-> do
                                            Logger.errorM iModuleName $ 
                                                    "Shutting down job reader thread. " 
                                                    `mappend` (show e) 
                                                    `mappend` " for " 
                                                    `mappend` (show f)
                                            handleDisconnects app 
                                                    connection nickN h)
                        jobReaderThread app nickN terminate
                        Logger.infoM iModuleName "Finished processing job " 
            Nothing -> jobReaderThread app nickN True  
        Logger.infoM iModuleName "Finished processing job" 


{-- The main processing loop for incoming commands.--}
writerThread :: App -> WSConn.Connection -> T.Text -> Bool -> IO ()
writerThread app connection nickName terminate = do
    if (terminate == True) 
        then do 
            Logger.infoM iModuleName "Writer thread exiting."
            return () 
        else do 
            msg <- WSConn.receiveData connection `catch` 
                (\h -> 
                    case h of 
                        (CloseRequest a b ) -> 
                            do 
                            _ <- handleDisconnects app connection nickName h
                            writerThread app connection nickName True
                            return "Close request received"                        
                        x -> do 
                            handleDisconnects app connection nickName h 
                            writerThread app connection nickName True 
                            return $ T.pack $ show x     

                )
            (result, nickName) <- liftIO $ getNickName $ incomingDictionary (msg :: T.Text)
            Logger.debugM iModuleName 
                            $ show $ msg  `mappend` nickName
            case result of 
                Nothing -> do 
                        liftIO $ WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                Just _ -> do 
                    liftIO $ Logger.infoM iModuleName 
                                $ "Writer thread :-> Message nickName " `mappend` (show nickName)
                    (dest, x) <- liftIO $ processIncomingMessage app connection nickName$ incomingDictionary msg
                    atomically $ do 
                                    clientStates <- case dest of 
                                        Broadcast -> getAllClients app nickName 
                                        PrivateMessage t ->
                                            getClientState t app
                                        _ ->
                                            getClientState nickName app                                        
                                    mapM_ (\cs -> writeTChan (writeChan cs) (x)) clientStates
                    writerThread app connection nickName terminate


getHomeR :: Handler Html
getHomeR = do
    request <- waiRequest
    liftIO $ Logger.infoM iModuleName 
                $ "Request " ++ (show request)
    webSockets ccarApp
    defaultLayout $ do
        [whamlet|
            <div #output>
            <form #form>
                <input #input autofocus>
        |]
        toWidget [lucius|
            \#output {
                width: 600px;
                height: 400px;
                border: 1px solid black;
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }
            \#input {
                width: 600px;
                display: block;
            }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                form = document.getElementById("form"),
                input = document.getElementById("input"),
                conn;

            
            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };

            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]


    
driver :: IO ()
driver = do
    sH <- openFile "debug.log" WriteMode
    hSetBuffering sH $ BlockBuffering $ Just 4096
    h <- SimpleLogger.streamHandler sH Log.DEBUG
    lh <- return $ setFormatter h (simpleLogFormatter "[$time : $loggername : $prio : $tid] $msg")
    s <- SimpleLogger.streamHandler stderr Log.ERROR
    _ <- Logger.updateGlobalLogger "CCAR" $ Logger.setLevel 
                                        Log.DEBUG . setHandlers[s, h, lh]
    
    Logger.debugM "CCAR" "Starting yesod.."                                    
    connStr <- getConnectionString
    poolSize <- getPoolSize
    runStderrLoggingT $ withPostgresqlPool connStr poolSize $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                runMigration migrateAll
    Logger.debugM "CCAR.Main.Driver" "Closing connection"
    chan <- atomically newBroadcastTChan
--    static@(Static settings) <- static "static"
    nickNameMap <- newTVarIO $ IMap.empty
    warp 3000 $ App chan  nickNameMap

