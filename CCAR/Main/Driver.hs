{--License: license.txt --}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module CCAR.Main.Driver
    (driver)
where 

import Data.Set as Set 
import Data.Map as Map
import Data.Ratio
import Yesod.Core
import Yesod.WebSockets as YWS
import Control.Monad.Trans.Control    (MonadBaseControl (liftBaseWith, restoreM))
import Network.WebSockets.Connection as WSConn
import Network.WebSockets 
import Yesod.Static
import Control.Exception hiding(Handler)
import qualified GHC.Conc as GHCConc
import CCAR.Parser.CCARParsec
import CCAR.Model.CcarDataTypes
import Control.Monad (forever, void, when, liftM, filterM, foldM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer 
import Control.Monad.Error
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async as A (waitSTM, wait, async, cancel, waitEither, waitBoth, waitAny
                        , concurrently,asyncThreadId)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Data.Time
import Data.Monoid ((<>), mappend)
import Control.Concurrent.STM.Lifted
import Data.Text as T  hiding(foldl, foldr)
import Data.Aeson as J
import Control.Exception(SomeException)
import Control.Applicative as Appl
import Data.Aeson.Encode as En
import Data.Aeson.Types as AeTypes(Result(..), parse)
import Data.Text.Lazy.Encoding as E
import Data.Text.Lazy as L hiding(foldl, foldr)
import System.IO
import Data.Map as Map 
import Data.HashMap.Lazy as LH (HashMap, lookup, member)
import qualified CCAR.Model.Person as Us 
import qualified CCAR.Model.CCAR as CCAR
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
import CCAR.Model.Country as Country
import CCAR.Model.Company as Company 
import CCAR.Model.Project as Project
import CCAR.Model.ProjectWorkbench as ProjectWorkbench
import CCAR.Model.Portfolio as Portfolio
import CCAR.Model.PortfolioSymbol as PortfolioSymbol
import CCAR.Entitlements.Entitlements as Entitlements
import CCAR.Data.TradierApi as TradierApi
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
import GHC.Conc(labelThread)
import Debug.Trace(traceEventIO)


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


data KeepAliveCommand = KeepAliveCommand {
        kaNickName :: T.Text
        , kaCommandType :: T.Text 
        , keepAlive :: T.Text } deriving(Show, Eq)

instance ToJSON KeepAliveCommand where 
    toJSON (KeepAliveCommand k1 k2 k3) = 
        object ["nickName" .= k1 
                , "commandType" .= k2 
                , "keepAlive" .= k3]

instance FromJSON KeepAliveCommand where 
    parseJSON (Object a) = KeepAliveCommand <$> 
        a .: "nickName" <*>
        a .: "commandType" <*>
        a .: "keepAlive"
    parseJSON _ = Appl.empty




type From = T.Text
type To = T.Text



data UserPreferences = UserPreferences {prefs :: T.Text} deriving (Show, Eq, Generic)


genPerson (Person a b c d e f) = object ["firstName" .= a
                                       , "lastName" .= b
                                       , "nickName" .= c
                                       , "password" .= d
                                       , "locale" .= e
                                       , "lastLoginTime" .= f]


genUserTermsOperations (UserTermsOperations o t) = object ["utOperation" .= o, "terms" .= t]

genTermsAndConditions (TermsAndConditions t des accept) = object ["title" .= t
                                            , "description" .= des
                                            , "acceptDate" .= accept]
genCommandKeepAlive a  = object ["KeepAlive" .= a
                                , "commandType" .= ("KeepAlive" :: T.Text)]



instance ToJSON UserTermsOperations where
    toJSON = genUserTermsOperations





commandType :: HashMap T.Text Value -> Maybe Value
commandType = LH.lookup "commandType"


parseKeepAlive v = v .: "keepAlive"

-- The upload 


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








iParseJSON :: (FromJSON a) => T.Text -> Either String (Maybe a)
iParseJSON = J.eitherDecode . E.encodeUtf8 . L.fromStrict

pJSON :: (FromJSON a) => T.Text -> IO (Either String (Maybe a))
pJSON  aText = do
    Logger.infoM  iModuleName ( T.unpack aText)
    return $ iParseJSON aText




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
            return $ (Reply, b {passwordValid = validatePassword (personPassword a) b
                                , numberOfAttemmpts = attempts + 1 })

validatePassword :: T.Text -> CheckPassword -> Maybe Bool 
validatePassword dbPassword input = Just $ dbPassword == (pwPassword input)







processCommandValue :: App -> T.Text -> Value -> IO (DestinationType, T.Text)
processCommandValue app nickName aValue@(Object a)   = do  
    case cType of 
        Nothing -> return $ (GroupCommunication.Reply, ser $ 
                                appError ("Unable to process command" :: T.Text))
        Just aType -> 
            case aType of 
                String "KeepAlive" ->
                        case (parse parseJSON aValue :: Result KeepAliveCommand) of
                            Success r -> return (GroupCommunication.Reply, ser r)
                            Error s -> 
                                return (
                                    GroupCommunication.Reply, 
                                    ser $ appError $ 
                                        "Parse Keep alive failed" ++ s ++ (show a))
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
                                (either :: Either ApplicationError 
                                    Entitlements.EntitlementT)
                                )
                String "QueryEntitlements" -> Entitlements.query nickName aValue 
                            >>= \(gc, either) -> 
                                    return (gc, 
                                        Util.serialize 
                                        (either :: 
                                                Either ApplicationError 
                                                Entitlements.QueryEntitlementT))
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
                                    Either ApplicationError 
                                        Entitlements.QueryCompanyEntitlementT))
                String "QueryCompanyUsers" -> Company.query nickName aValue 
                            >>= \(gc, either) -> 
                                return (gc, Util.serialize 
                                        (either :: Either ApplicationError 
                                                    Company.QueryCompanyUsers))
                String "QueryOptionChain" -> TradierApi.query nickName aValue
                            >>= \(gc, either) -> 
                                return (gc, Util.serialize 
                                        (either :: Either ApplicationError 
                                                        TradierApi.QueryOptionChain))
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
                String "CCARUpload" -> 
                        CCAR.manage nickName aValue
                            >>= \(gc, either) -> 
                                return (gc, Util.serialize 
                                        (either :: Either ApplicationError CCAR.CCARUpload)) 
                String "ParsedCCARText" -> do 
                        (gc, x) <- CCAR.parseCCMessage nickName aValue 
                        case x of 
                            Left x -> return (gc, Util.serialize x) 
                            Right (CCAR.CCARText uploadedBy name y) -> do
                                Logger.infoM iModuleName "Calling parsed ccar text..." 
                                y2 <- return $ readExprTree y 
                                Logger.debugM iModuleName $ "Scenarios " ++ (show y2)
                                atomically $ updateActiveScenario app nickName y2
                                cState <- atomically $ getClientState nickName app
                                Logger.debugM iModuleName $ show cState
                                return (gc, Util.serialize y)

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
                                    , ser $ appError $ "parse manage user failed " ++ s )
                _ ->                                                
                    return 
                         ( GroupCommunication.Reply
                         , ser $ appError ("Unable to process command " ++ (show aType)))
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
                appError ("Unknown error" :: T.Text))
        Just (Object a) -> 
              case nn of
                Nothing -> return (Nothing, "Nickname tag not found")
                Just x -> 
                    case x of
                        String x -> return (Just "found nickName", x)
                        _ -> return (Nothing, T.pack $ "Invalid " ++ (show x))
            where 
                nn = LH.lookup "nickName" a 




processLoginMessages :: App -> WSConn.Connection -> T.Text -> Maybe Value -> IO (DestinationType, T.Text)
processLoginMessages app conn aNickName aDictionary = do 
    x <- runMaybeT $ do 
        Just (Object a) <- return aDictionary
        Just commandType <- return $ LH.lookup "commandType" a 
        case commandType of 
            String "Login" -> 
                        liftIO $ Login.query aNickName (Object a)
                        >>= \(gc, result) -> 
                        return (gc, 
                            Util.serialize
                            (result ::Either ApplicationError Login))
    case x of 
        Just y -> return y
        Nothing -> return (GroupCommunication.Reply, 
                            ser $ appError ("Error processing login messages." :: String))
processIncomingMessage :: App -> WSConn.Connection -> T.Text ->  Maybe Value -> IO (DestinationType , T.Text)
processIncomingMessage app conn aNickName aCommand = do 
    case aCommand of 
        Nothing -> do 
                (x, y) <- runWriterT $ do 
                    tell [("Processing error " ++ (show aNickName))]
                    
                    result <- return (appError ("Unknown error" :: T.Text)) 
                    return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode  result)
                Logger.errorM iModuleName  (show y)
                return x                     
        Just (Object a) -> do 
                 (processCommandValue app aNickName (Object a)) `catch`
                    (\e -> do
                            Logger.errorM iModuleName $  ("Exception "  ++ show (e :: PersistException)) 
                            atomically $ deleteConnection app aNickName
                            return (GroupCommunication.Broadcast, 
                                    Util.serialize $ UserJoined.userLeft aNickName)
                            )
                --return $ (d, L.toStrict $ E.decodeUtf8 $ En.encode command)
                    


getActiveScenario :: App -> T.Text -> STM [Stress]
getActiveScenario app nn = do 
    cMap <- readTVar $ nickNameMap app 
    clientState <- return $ IMap.lookup nn cMap 
    case clientState of 
            Nothing -> return [] 
            Just x1 -> return $ activeScenario x1

updateActiveScenario :: App -> T.Text -> [Stress] -> STM()
updateActiveScenario app nn x = do 
    cMap <- readTVar $ nickNameMap app 
    clientState <- return $ IMap.lookup nn cMap
    case clientState of 
            Nothing -> return () 
            Just x1 -> do 
                _ <- writeTVar (nickNameMap app) 
                            (IMap.insert nn (x1 {activeScenario = x}) (cMap))
                return ()

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

getAllClientIdentifiers :: App -> STM [ClientIdentifier]
getAllClientIdentifiers app@(App a c) = do 
    nMap <- readTVar c 
    return $ Map.keys nMap


countAllClients :: App ->  STM Int 
countAllClients app@(App a c) = do
    nMap <- readTVar c 
    return $ Map.size nMap

getAllClients :: App -> T.Text -> STM [ClientState]
getAllClients app@(App a c) nn = do
    nMap <- readTVar c 
    return $ Prelude.filter (\x -> nn /= (nickName x)) $ IMap.elems nMap 
getClientState :: T.Text -> App -> STM [ClientState]
getClientState nickName app@(App a c) = do
        nMap <- readTVar c
        if IMap.member nickName nMap then 
            return $ [nMap ! nickName]
        else 
            return [] 

getPersonNickName :: Maybe Person -> Maybe T.Text
getPersonNickName a = do
    case a of 
        Just x -> return $  personNickName x

authenticate :: WSConn.Connection -> T.Text -> App -> IO (DestinationType, T.Text)
authenticate aConn aText app@(App a c) = 
    do 
        case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                        L.toStrict $ E.decodeUtf8 $ En.encode 
                                $ appError ("Invalid command during login" :: T.Text))
            Just o@(Object a) -> do                
                result <- return $ (parse parseJSON o :: Result Login)
                case result of 
                    Success (r@(Login a b)) -> do 
                            x <- runMaybeT $ do 
                                Just nickName <- return $ getPersonNickName a 
                                return $ UserJoined.userJoined nickName
                            case x of 
                                Nothing -> return(GroupCommunication.Reply, 
                                    ser $ appError ("Invalid user name" :: T.Text))
                                Just y -> return (GroupCommunication.Reply, y)
                    Error s -> 
                        return (GroupCommunication.Reply, T.pack s)
        where 
            aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value



ser  = (L.toStrict) . (E.decodeUtf8) . (En.encode)

processUserLoggedIn :: WSConn.Connection -> T.Text -> App -> T.Text -> IO (DestinationType, T.Text) 
processUserLoggedIn aConn aText app@(App a c) nickName = do
    case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                    ser $ appError ("Login has errors" :: T.Text))
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
                                                $ appError ("Invalid command during login" :: T.Text))                                
                    String "UserJoined" -> do 
                        c <- return $ (parse parseJSON o :: Result UserJoined)
                        case c of 
                            Success u@(UserJoined.UserJoined a) -> do 
                                        return $ (Broadcast, ser u)
                            _ -> return $ (GroupCommunication.Reply, ser  
                                                $ appError 
                                                    ("Invalid command during login" :: T.Text))
                    String "ManageUser"-> do
                        Logger.debugM iModuleName  ("Manage user " `mappend`   
                                                        (T.unpack $ aText))
                        g@(gc, res) <- UserOperations.manage aText o 
                        case res of 
                            Right x -> atomically $ addConnection app aConn nickName
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
                                        , ser $ appError
                                                $ ("Guest login failed ") `mappend`  errMessage )

                    String "Login" -> return (GroupCommunication.Reply, aText) -- tuple the input up
                    _ -> return (GroupCommunication.Reply 
                                , ser $ appError 
                                        $ "process user logged in failed :  " ++ (show aText) )

            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value


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
                liftIO $ Logger.debugM iModuleName  $ "Incoming text " `mappend` (T.unpack(command :: T.Text))
                processResult <- case result of 
                    Nothing -> do 
                            $(logInfo) command 
                            liftIO $ do  
                                        _ <- WSConn.sendClose connection 
                                            ("Nick name tag is mandatory. Bye" :: T.Text)
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
                                            d <- (A.async $ liftIO $ runner TradierServer app connection nickNameV False)
                                            labelThread (A.asyncThreadId a) 
                                                        ("Writer thread " ++ (T.unpack nickNameV))
                                            labelThread (A.asyncThreadId b) 
                                                    ("Reader thread " ++ (T.unpack nickNameV))
                                            labelThread (A.asyncThreadId c) 
                                                    ("Job thread " ++ (T.unpack nickNameV))
                                            labelThread (A.asyncThreadId d)
                                                    ("Market data thread " ++ (T.unpack nickNameV))
                                            A.waitAny [a,  b,  c, d ]
                                            return "Threads had exception") 
                            return ("All threads exited" :: T.Text)
                return () 
            _ -> liftIO $ WSConn.sendClose connection 
                    ("Active connection. Multiple logins not allowed. " `mappend` nickNameV)



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
            (dest, text) <- liftIO $ processUserLoggedIn connection command app nickNameV
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


{-- | Market data thread polls for the market data values for all the symbols
    | across all the portfolios for the user.
 --}

{-- The main processing loop for incoming commands.--}
writerThread :: App -> WSConn.Connection -> T.Text -> Bool -> IO ()
writerThread app connection nickName terminate = do
    Logger.debugM iModuleName $ 
        (show connection) ++ "->" ++  (T.unpack nickName)
    if (terminate == True) 
        then do 
            Logger.infoM iModuleName "Writer thread exiting."
            return () 
        else do 
            liftIO $ Logger.debugM iModuleName "Waiting for message writerThread..."
            traceEventIO "Before reading connection data"
            allClients <- atomically $ countAllClients app 
            allClientIds <- atomically $ getAllClientIdentifiers app
            liftIO $ Logger.debugM iModuleName $ ("Clients connected " ++ (show allClients))
            liftIO $ Logger.debugM iModuleName $ ("Client identifiers " ++ (show allClientIds))
            msg <- WSConn.receiveData connection `catch` 
                (\h -> 
                    case h of 
                        (CloseRequest a b ) -> 
                            do 
                            _ <- handleDisconnects app connection nickName h
                            Logger.errorM iModuleName  "Close request received"                        
                            return "Close request received"
                        _ -> do 
                            handleDisconnects app connection nickName h 
                            Logger.errorM iModuleName "Unknown exception"
                            return $ T.pack $ "Unknown exception"

                )
            liftIO $ Logger.debugM iModuleName ("Reading message from the connection " ++ (T.unpack msg))
            (result, nickName) <- liftIO $ getNickName $ incomingDictionary (msg :: T.Text)
            case result of 
                Nothing -> do 
                        liftIO $ WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                Just _ -> do 
                    (dest, x) <- liftIO $ processIncomingMessage app connection nickName 
                                                $ incomingDictionary msg
                    
                    liftIO $ Logger.debugM iModuleName ("Destination " ++ (show dest))
                    atomically $ do 
                                    clientStates <- case dest of 
                                        Broadcast -> getAllClients app nickName 
                                        PrivateMessage t ->
                                            getClientState t app
                                        _ ->
                                            getClientState nickName app                                        
                                    mapM_ (\cs -> writeTChan (writeChan cs) (x)) clientStates
                    WSConn.sendPing connection ("ping" :: T.Text)
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
--    s <- SimpleLogger.streamHandler stderr Log.ERROR
    _ <- Logger.updateGlobalLogger "CCAR" $ Logger.setLevel 
                                        Log.DEBUG . setHandlers[lh]
    
    Logger.debugM "CCAR" "Starting yesod.."                                    
    connStr <- getConnectionString
    poolSize <- getPoolSize
    runStderrLoggingT $ withPostgresqlPool connStr poolSize $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                runMigration ccarModel
    c <- A.async $ Country.startup
    t <- A.async $ do 
        TradierApi.startup
    chan <- atomically newBroadcastTChan
--    static@(Static settings) <- static "static"
    nickNameMap <- newTVarIO $ IMap.empty
    warp 3000 $ App chan  nickNameMap




class MarketDataServer a where 
    {-- | A polling interval to poll for data. Non real time threads.--}
    realtime :: a -> IO Bool 
    pollingInterval :: a -> IO Int 
    runner :: a -> App -> WSConn.Connection -> T.Text -> Bool -> IO ()

data TradierMarketDataServer = TradierServer 


instance MarketDataServer TradierMarketDataServer where 
    realtime a = return False
    pollingInterval a = tradierPollingInterval 
    runner i a n t = tradierRunner a n t 


toDouble :: StressValue -> Double 
toDouble (Percentage Positive x) =  fromRational x 
toDouble (Percentage Negative x) =  -1 * (fromRational x)
_                                = 0.0 -- Need to model this better.


computeValue :: MarketData -> PortfolioSymbol -> [Stress] -> IO T.Text
computeValue a b stress = do 
        m <- return $ T.unpack (marketDataLastPrice a )
        q <- return $ T.unpack (portfolioSymbolQuantity b)
        mD <- return $ (read m :: Double)
        qD <- return $ (read q :: Double)
        stressM <- return stress 
        sVT <- foldM (\sValue s -> 
                case s of 
                    EquityStress (Equity s1) sV -> return $ sValue + (toDouble sV)
                    _ -> return sValue) 0.0 stressM 
        Logger.debugM iModuleName $ "Total stress " ++ (show sVT)
        return $ T.pack $ show (mD * qD * (1 - sVT))
-- Refactoring note: move this to market data api.
-- The method is too complex. Need to fix it.
-- High level: 
-- Get all the symbols for the users portfolio,
-- Send a portfolio update : query the portfolio object
-- get the uuid and then map over it.
tradierPollingInterval :: IO Int 
tradierPollingInterval = return $ 10 * 10 ^ 6
tradierRunner :: App -> WSConn.Connection -> T.Text -> Bool -> IO ()
tradierRunner app conn nickName terminate = 
    if(terminate == True) then do 
        Logger.infoM iModuleName "Market data thread exiting" 
        return ()
    else do 
        Logger.debugM iModuleName "Waiting for data"
        tradierPollingInterval >>= \x -> threadDelay x        
        mySymbols <- Portfolio.queryUniqueSymbols nickName
        marketDataMap <- TradierApi.queryMarketData
        portfolioIds <- mapM (\p -> return $ portfolioSymbolPortfolio p) mySymbols
        pSet <- return $ Set.fromList portfolioIds
        portfoliom  <- mapM (\x -> do 
                        p <- dbOps $  DB.get x 
                        case p of 
                            Just x2 -> return (x, portfolioUuid x2)
                            Nothing -> return (x, "INVALID PORTFOLIO")) 
                        (Set.toList pSet)
        portfolioMap <- return $ Map.fromList portfoliom
        upd <- mapM (\x -> do 
                activeScenario <- liftIO $ atomically $ getActiveScenario app nickName 
                Logger.infoM iModuleName $ " Active scenario " ++ (show activeScenario)
                val <- return $ Map.lookup (portfolioSymbolSymbol x) marketDataMap 
                (stressValue, p) <- case val of 
                        Just v -> do 
                            c <- computeValue v x activeScenario
                            return (c, x {portfolioSymbolValue = marketDataLastPrice v}) 
                        Nothing -> return ("0.0", x)
                x2 <- return $ Map.lookup (portfolioSymbolPortfolio x) portfolioMap 
                pid <- case x2 of 
                    Nothing -> return "INVALID PORTFOLIO" 
                    Just y -> return y                  
                return $ daoToDto PortfolioSymbol.P_Update pid 
                            nickName nickName nickName p stressValue

            ) mySymbols

        mapM_  (\p -> do
                liftIO $ Logger.debugM iModuleName ("test" `mappend` (show $ Util.serialize p))
                liftIO $ threadDelay $ 1 * 10 ^ 4
                liftIO $ WSConn.sendTextData conn (Util.serialize p) 
                return p 
                ) upd 
        tradierRunner app conn nickName False
