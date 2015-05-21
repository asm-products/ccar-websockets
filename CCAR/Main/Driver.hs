{--License: license.txt --}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module CCAR.Main.Driver
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
import Control.Concurrent.Async as A (waitSTM, wait, async, cancel, waitEither, waitBoth
                        , concurrently)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(runStderrLoggingT)
import Data.Time
import Conduit
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
import Data.ByteString as DBS hiding (putStrLn)
import Data.ByteString.Char8 as C8 hiding(putStrLn) 
import System.Environment

import CCAR.Main.Util
import GHC.Generics
import Data.Data
import Data.Typeable 
import Database.Persist.Postgresql as DB
import Database.Persist 
import Data.Map as IMap
import CCAR.Main.DBUtils
import CCAR.Main.GroupCommunication as GroupCommunication
import CCAR.Main.UserJoined as UserJoined 
import CCAR.Command.ErrorCommand 
import CCAR.Model.Person
import CCAR.Model.Company as Company 
import CCAR.Model.Project as Project
import CCAR.Model.ProjectWorkbench as ProjectWorkbench

--connStr = "host=localhost dbname=ccar_debug user=ccar password=ccar port=5432"
connStr = getConnectionString

data LoginStatus = UserExists | UserNotFound | InvalidPassword | Undefined | Guest
    deriving(Show, Typeable, Data, Generic, Eq)


data Login  =    Login {login :: Maybe Person, loginStatus :: Maybe LoginStatus} 
                deriving (Show, Eq)

data UserOperations = UserOperations{operation :: Us.CRUD, person :: Maybe Person} 
                deriving (Show, Eq)
data UserTermsOperations = UserTermsOperations {utOperation :: Ust.CRUD, terms :: Maybe TermsAndConditions} 
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
data Command = CommandLogin Login 
                | CommandUO UserOperations 
                | CommandUTO UserTermsOperations
                | CommandCCARUpload CCARUpload
                | CommandError ErrorCommand 
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
genLogin  (Login a b) = object [
    "commandType" .= (String "Login")
    , "login" .= Just a, "loginStatus" .= b]

genUserOperations (UserOperations o p) = object [
                        "operation" .= o
                        , "person" .= p
                        , "commandType" .= (String "ManageUser")]
genUserTermsOperations (UserTermsOperations o t) = object ["utOperation" .= o, "terms" .= t]

genTermsAndConditions (TermsAndConditions t des accept) = object ["title" .= t
                                            , "description" .= des
                                            , "acceptDate" .= accept]
genCommandKeepAlive a  = object ["KeepAlive" .= a
                                , "commandType" .= ("KeepAlive" :: T.Text)]


instance ToJSON Login where
    toJSON = genLogin 
instance ToJSON UserOperations where
    toJSON = genUserOperations

instance ToJSON UserTermsOperations where
    toJSON = genUserTermsOperations

instance ToJSON TermsAndConditions where
    toJSON = genTermsAndConditions

instance ToJSON CCARUpload where
    toJSON = genCCARUpload 

instance ToJSON CCAR where
    toJSON = genCCAR 

instance ToJSON Command where
    toJSON aCommand = 
        case aCommand of
            CommandLogin l -> genLogin l 
            CommandError e -> genErrorCommand e
            CommandUO e -> genUserOperations e
            CommandCCARUpload a  -> genCCARUpload a
            CommandKeepAlive a -> genCommandKeepAlive a
            ParseCCARText a -> toJSON a
            _ -> genErrorCommand $ ErrorCommand {errorCode = "Unknown" :: T.Text , 
                        message = T.pack (show aCommand)}



commandType = LH.lookup "commandType"

parseCommand value = do
        case (commandType value) of
            Nothing -> CommandError <$> parseErrorCommand value
            Just cType -> 
                case (cType) of 
                    "Login"-> CommandLogin <$> parseLogin value
                    "ManageUser" -> CommandUO <$> parseCreateUser value
                    "CCARUpload" -> CommandCCARUpload <$> parseCCARUpload value
                    "KeepAlive" -> CommandKeepAlive <$> parseKeepAlive value
                    "ParsedCCARText" -> ParseCCARText <$> parseCCARText value
                    _       -> CommandError <$> parseErrorCommand value


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


parseCreateUser v = UserOperations <$>
                        v .: "operation" <*> 
                        v .: "person"
parseLogin v = Login <$> 
                v .: "login" <*>
                (v .: "loginStatus")

parseTermsAndConditions v = TermsAndConditions <$>
                        v .: "title" <*>
                        v .: "description" <*>
                        v .: "acceptDate"

parseCCARText v = CCARText <$>
                    v .: "uploadedBy" <*>
                    v .: "scenarioName" <*>
                    v .: "ccarText"


instance FromJSON Login where
    parseJSON (Object v) = parseLogin v
    parseJSON _          = Appl.empty


instance FromJSON TermsAndConditions where
    parseJSON (Object v) = parseTermsAndConditions v
    parseJSON _          = Appl.empty

instance FromJSON CCAR where 
    parseJSON (Object v) = parseCCAR v
    parseJSON _          = Appl.empty

instance FromJSON Command where
    parseJSON (Object v) = parseCommand v
    parseJSON _         = Appl.empty

instance FromJSON CCARText where
    parseJSON (Object v) = parseCCARText  v 
    parseJSON _          = Appl.empty


iParseJSON :: (FromJSON a) => T.Text -> Either String (Maybe a)
iParseJSON = J.eitherDecode . E.encodeUtf8 . L.fromStrict

pJSON :: (FromJSON a) => T.Text -> IO (Either String (Maybe a))
pJSON  aText = do
    --putStrLn $  "pJSON " ++ (T.unpack aText)
    return $ iParseJSON aText

decoder :: Command -> T.Text 
decoder = L.toStrict . E.decodeUtf8 . En.encode        
decoderM a = return $ L.toStrict $ E.decodeUtf8 $ En.encode a



type NickName = T.Text
type ClientMap = GroupCommunication.ClientIdentifierMap

-- the broadcast channel for the application.
data App = App { chan :: (TChan T.Text)
                , getStatic :: Static
                , nickNameMap :: ClientMap}

instance Yesod App

mkYesod "App" [parseRoutes|
/chat HomeR GET
/portNumber PortNumberR GET
/static StaticR  Static getStatic
|]


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
processCommand (Just (CommandLogin aLogin)) = do 
    p <- case (login aLogin) of
            Just a -> return a 
    chk <- checkLoginExists (personNickName p)
    putStrLn $ show $ "Login exists " ++ (show chk)
    case chk of 
        Nothing -> return $ (GroupCommunication.Reply, 
                CommandLogin $ Login {login = Just p, 
               loginStatus = Just UserNotFound})
        Just (Entity aid a)  -> do 
                _ <- updateLogin a 
                return $ (Broadcast, CommandLogin $ 
                    Login {login = Just a, loginStatus = Just UserExists})


processCommand (Just (CommandError error)) = 
        return $ (GroupCommunication.Reply, CommandError error)

processCommand Nothing = return $  
                        (GroupCommunication.Reply, CommandError $ 
                            genericErrorCommand "Unable to process command")

processCommand (Just (CommandKeepAlive a)) = 
        return $ (GroupCommunication.Reply, CommandKeepAlive a)

processCommand (Just ( CommandUO (UserOperations uo aPerson))) = do
    putStrLn $ show $ "Processing processCommand " ++ (show uo)
    person <- case aPerson of
                Just a -> return a        
    case uo of
        Us.Create  -> do
                personId <- insertPerson person
                putStrLn $ show $ "Person inserted " ++ (show personId)
                return $ (GroupCommunication.Reply, CommandUO $ UserOperations Us.Create (Just person))
        Us.Update personId -> do
                updatePerson personId person
                return $ (GroupCommunication.Reply, CommandUO 
                            $ UserOperations (Us.Update personId) (Just person))
        Us.Delete personId -> do 
                deletePerson personId person
                return $ (GroupCommunication.Reply, CommandUO $ UserOperations (Us.Delete personId) (Just person))
        Us.Query personId -> do 
                maybePerson <- queryPerson (personId)
                case maybePerson of
                    Nothing -> 
                        return $ (GroupCommunication.Reply, CommandUO  $ UserOperations (Us.Query personId) Nothing)
                    Just (p) -> 
                        return $ (GroupCommunication.Reply, CommandUO  $ UserOperations (Us.Query personId) (Just p))

-- | Query operations are replies and db operations are broadcast. | --
processCommand (Just (CommandCCARUpload (CCARUpload nickName operation aCCAR aList))) = do
    putStrLn $ show $ "Processing command ccar upload " ++ (show ccar)
    case operation of 
        CC.Create -> do 
                ccarId <- insertCCAR ccar
                putStrLn $ show $ "CCAR created " ++ (show ccar)
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

processCommandValue :: App -> WSConn.Connection -> T.Text -> Value -> IO (DestinationType, T.Text)
processCommandValue app aConn nickName (Object a)   = do  
    case cType of 
        Nothing -> return $ (GroupCommunication.Reply, ser $ 
                        CommandError $ genericErrorCommand "Unable to process command")
        Just aType -> 
            case aType of 
                String "Login" -> 
                        case (parse parseLogin a) of
                            Success r -> do 
                                    (d, c) <- processCommand $ (Just (CommandLogin r))
                                    return (d, ser c)
                            Error s -> 
                                    return (GroupCommunication.Reply,
                                            ser $ CommandError $ genericErrorCommand $ "parse login  failed "++ s)
                String "ManageUser" ->
                        case (parse parseCreateUser a) of
                            -- Assert that the user operation is not an insert for the person table.
                            Success r -> do
                                    (d, c) <- processCommand $ Just $ CommandUO r
                                    return (d, ser c)
                            Error s -> 
                                return (GroupCommunication.Reply 
                                    , ser $ CommandError $ genericErrorCommand $ "parse manage user failed " ++ s )
                String "UserBanned" -> do
                        c <- return $ parse UserJoined.parseUserBanned a 
                        case c of
                            Success u@(UserJoined.UserBanned a1) -> do
                                bConns <- atomically $ getClientState a1 app 
                                mapM_ (\bconn -> WSConn.sendClose (connection bconn)
                                        ("Bye"
                                            :: T.Text)) bConns  -- To handle multiple connections to a client.
                                return (GroupCommunication.Broadcast, ser u)
                            Error s ->  return (GroupCommunication.Reply 
                                    , ser $ CommandError $ genericErrorCommand $ "parse manage user failed " ++ s )

                String "CCARUpload" ->
                        case (parse parseCCARUpload a) of
                            Success r -> do  
                                    (d, c) <- processCommand $ Just $ CommandCCARUpload r 
                                    return (d, ser c)
                            Error s -> 
                                return (GroupCommunication.Reply 
                                    , ser $ 
                                        CommandError $ genericErrorCommand $ "parse ccar upload failed " ++ s ++ (show a))
                String "KeepAlive" ->
                        case (parse parseKeepAlive a) of
                            Success r -> do 
                                    (d, c) <- processCommand $ Just $ CommandKeepAlive r 
                                    return (d, ser c)
                            Error s -> 
                                return (
                                    GroupCommunication.Reply, 
                                    ser $ CommandError $ genericErrorCommand $ 
                                        "Parse Keep alive failed" ++ s ++ (show a))
                String "ParsedCCARText" ->
                        case (parse parseCCARText a) of
                            Success r -> do 
                                    (d, c) <- processCommand $ Just $ ParseCCARText r 
                                    return (d, ser c)
                            Error s -> 
                                return (
                                    GroupCommunication.Reply
                                    , ser $ CommandError $ genericErrorCommand $ "Parse CCAR Text " ++ s ++ (show a)
                                )
                String "SendMessage" -> processSendMessage (Object a)
                String "ManageCompany" -> Company.manageCompany nickName (Object a)
                String "SelectAllCompanies" -> Company.queryAllCompanies nickName (Object a)
                String "ManageProject" -> Project.manageProject nickName (Object a)
                String "ManageSurvey" -> Survey.processManageSurvey (Object a) 
                String "SelectActiveProjects" -> Project.queryActiveProjects nickName (Object a)
                String "QuerySupportedScripts" -> ProjectWorkbench.querySupportedScripts nickName (Object a)
                String "QueryActiveWorkbenches" -> ProjectWorkbench.queryActiveWorkbenches (Object a)
                String "ManageWorkbench" -> ProjectWorkbench.manageWorkbench (Object a)
                String "ExecuteWorkbench" -> ProjectWorkbench.executeWorkbench(Object a)
                _ -> 
                    return 
                         ( GroupCommunication.Reply
                         , ser $ CommandError $ genericErrorCommand ("Unable to process command " ++ (show aType)))
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
            Nothing -> return $ (Nothing, L.toStrict $ E.decodeUtf8 $ En.encode $ CommandError $ genericErrorCommand "Unknown error")
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
    --putStrLn $  "Processing incoming message " ++ (show aCommand)
    case aCommand of 
        Nothing -> do
                putStrLn $ "Processing error..."
                result <- return (CommandError $ genericErrorCommand ("Unknown error")) 
                return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode  result)
                    
        Just (Object a) -> do 
                putStrLn $ show $ "Processing command type " ++ (show (LH.lookup "commandType" a))
                (processCommandValue app conn aNickName (Object a)) `catch`
                    (\e -> do
                            putStrLn $  ("Exception "  ++ show (e :: PersistException)) 
                            atomically $ deleteConnection app conn aNickName
                            return (GroupCommunication.Broadcast, 
                                    serialize $ UserJoined.userLeft aNickName)
                            )
                --return $ (d, L.toStrict $ E.decodeUtf8 $ En.encode command)
                    


deleteConnection :: App -> WSConn.Connection -> T.Text -> STM  () 
deleteConnection app conn nn = do 
            cMap <- readTVar $ nickNameMap app                
            _ <-    writeTVar (nickNameMap app) (IMap.delete nn cMap)
            return ()

addConnection :: App -> WSConn.Connection ->  T.Text -> STM ()
addConnection app aConn nn = do 
                nMap <- readTVar $ nickNameMap app 
                w <- newTChan
                r <- dupTChan w 
                clientState <- return ClientState{nickName = nn 
                        , connection = aConn
                        , readChan = r 
                        , writeChan = w 
                }
                _ <- writeTVar (nickNameMap app) (IMap.insert nn clientState nMap)
                return ()


getAllClients :: App -> T.Text -> STM [ClientState]
getAllClients app@(App a b c ) nn = do
    nMap <- readTVar c 
    return $ Prelude.filter (\x -> nn /= (nickName x)) $ elems nMap 
getClientState :: T.Text -> App -> STM [ClientState]
getClientState nickName app@(App a b c) = do
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
authenticate aConn aText app@(App a b c) = 
    do 
        case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                        L.toStrict $ E.decodeUtf8 $ En.encode $ CommandError 
                                $ genericErrorCommand ("Invalid command during login"))
            Just (Object a) -> do
                (d, c) <- processCommandValue app aConn aText (Object a) 
                c1 <- return $ (J.decode $ E.encodeUtf8 $ L.fromStrict c :: Maybe Value)
                case c1 of 
                    Just (Object a ) -> do  
                        result <- return $ parse parseLogin a 
                        case result of 
                            Success (r@(Login a b)) -> do 
                                    nickName <- getPersonNickName a 
                                    userJoined <- return $ UserJoined.userJoined nickName 
                                    return (GroupCommunication.Reply, userJoined)
        where 
            aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value




processUserLoggedIn :: WSConn.Connection -> T.Text -> App -> IO (DestinationType, T.Text) 
processUserLoggedIn aConn aText app@(App a b c) = do
    case aCommand of 
            Nothing -> return (GroupCommunication.Reply, 
                    ser $ CommandError $ genericErrorCommand ("Login has errors"))
            Just (Object a) -> do
                Just commandType <- return $ LH.lookup "commandType" a
                case commandType of 
                    String "UserLoggedIn" -> do 
                        c <- return $ parse UserJoined.parseUserLoggedIn a  
                        case c of 
                            Success u@(UserJoined.UserLoggedIn a) -> do 
                                        atomically $ addConnection app aConn a 
                                        return $ (Broadcast, ser u)
                            _ -> return $ (GroupCommunication.Reply, ser  
                                                $ CommandError $ 
                                                genericErrorCommand ("Invalid command during login"))                                
                    String "UserJoined" -> do 
                        c <- return $ parse UserJoined.parseUserJoined a  
                        case c of 
                            Success u@(UserJoined.UserJoined a) -> do 
                                        return $ (Broadcast, ser u)
                            _ -> return $ (GroupCommunication.Reply, ser  
                                                $ CommandError $ 
                                                genericErrorCommand ("Invalid command during login"))
                    String "ManageUser"-> do 
                        c2 <- return $ (parse parseCreateUser a)
                        case c2 of 
                            Success r -> do
                                    (d, cuo@(
                                            CommandUO (UserOperations opType (Just c1)))) <- do 
                                        processCommand $ Just $ CommandUO r
                                    atomically $ addConnection app aConn $ personNickName c1
                                    return (d, ser cuo)
                            Error s -> 
                                return (GroupCommunication.Reply 
                                    , ser $ CommandError $ genericErrorCommand 
                                            $ "parse manage user failed " ++ s )
                    String "GuestUser" -> do 
                        result <- return $ (parse parseGuestUser a)
                        case result of 
                            Success (g@(UserJoined.GuestUser guestNickName)) -> do
                                    createGuestLogin guestNickName  
                                    atomically $ addConnection app aConn guestNickName
                                    return $ (GroupCommunication.Broadcast, ser g)                          
                            Error errMessage ->  
                                    return (GroupCommunication.Reply 
                                        , ser $ CommandError $ genericErrorCommand
                                                $ ("Guest login failed ") `mappend`  errMessage )
                    _ -> return (GroupCommunication.Reply 
                                , ser $ CommandError $ genericErrorCommand 
                                        $ "process user logged in failed :  " ++ (show aText) )

            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value
                ser  = (L.toStrict) . (E.decodeUtf8) . (En.encode)

instance Show WSConn.Connection where
    show (WSConn.Connection o cType proto msgIn msgOut cl c2) = show proto 

-- Do not let multiple connections to the same nick name.
-- How do we allow multiple connections for handling larger data, such as
-- video or file upload?
-- Create a session id for a connection and send that token back to the client.
-- Subsequent request for another connection needs to be assigned the same token. 


ccarApp :: WebSocketsT Handler ()
ccarApp = do
        connection <- ask
        app <- getYesod
        liftIO $ putStrLn "Before receiving data..."
        command <- liftIO $ WSConn.receiveData connection
        --liftIO $ putStrLn $ show (command :: T.Text)
        (result, nickNameV) <- liftIO $ getNickName $ incomingDictionary (command :: T.Text)
        clientState <- atomically $ getClientState nickNameV app
        liftIO $ putStrLn "Before showing client state"
        liftIO $ putStrLn $ show clientState 
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
                                        atomically $ deleteConnection app connection nickNameV
                                        return "Close request" )
                            a <- liftBaseWith (\run -> run $ liftIO $ do
                                            a <- (A.async (writerThread app connection 
                                                nickNameV False))
                                            b <- (A.async (liftIO $ readerThread app nickNameV False))
                                            A.waitEither a b
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
                    liftIO $ putStrLn $ "Sending " ++ ( show nickNameFound)
                    WSConn.sendTextData connection nickNameFound
                    command <- (processClientLeft connection app nickNameV ) `catch` 
                                                (\ (CloseRequest e1 e2) -> do                                                             
                                                        atomically $ deleteConnection app connection nickNameV
                                                        return (UserJoined.userLeft nickNameV)
                                                    )
                    return ("Threads exited" :: T.Text)
    
{-- Client hits a refresh or loses connection --}
processClientLeft connection app nickNameV = do
            command <- WSConn.receiveData connection
            (dest, text) <- liftIO $ processUserLoggedIn connection command app 
            putStrLn $ "User logged in " ++ (show text)
--            putStrLn $ "Incoming command " ++ (show command)
            messageLimit <- liftIO $ getMessageCount nickNameV
            putStrLn $ "Using message limit " ++ (show messageLimit)
            messageHistory <- liftIO $ GroupCommunication.getMessageHistory messageLimit
            putStrLn $ "After messageHistory " ++ (show nickNameV)
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
            putStrLn $ T.unpack $  
                ("Bye nickName " :: T.Text) `mappend` nickN
            atomically $ do 
                deleteConnection app connection nickN
                restOfUs <- getAllClients app nickN
                case restOfUs of 
                    x  : _ -> do  
                        mapM_ (\cs -> 
                                writeTChan(writeChan cs) $ 
                                    UserJoined.userLeft nickN )restOfUs
                    [] -> return ()
            
readerThread app nickN terminate = do
    if (terminate == True) 
        then do 
            putStrLn "Reader thread exiting" 
            return () 
    else do 
        putStrLn "Waiting for messages..."
        (conn , textData) <- atomically $ do
                clientStates <- getClientState nickN app 
                case clientStates  of 
                    clientState : _ -> do 
                        textData <- readTChan (readChan clientState)                        
                        return (Just $ connection clientState, textData)
                    [] -> return (Nothing, "")
        case conn of 
            Just connection -> do 
                                _ <- WSConn.sendTextData (connection) textData `catch` 
                                        (\h@(CloseRequest e f)-> handleDisconnects app 
                                                    connection nickN h)
                                readerThread app nickN terminate
            Nothing -> readerThread app nickN True  
        --liftIO $ putStrLn $ "Wrote " `mappend` (show textData) `mappend` (show conn)
        


{-- The main processing loop for incoming commands.--}
writerThread app connection nickName terminate = do
    if (terminate == True) 
        then do 
            putStrLn "Writer thread exiting."
            return () 
        else do 
            msg <- WSConn.receiveData connection `catch` 
                (\h@(CloseRequest _ _) -> do 
                        _ <- handleDisconnects app connection nickName h
                        writerThread app connection nickName True
                        return "Close request received")
            (result, nickName) <- liftIO $ getNickName $ incomingDictionary (msg :: T.Text)
            --putStrLn $ show $ msg  `mappend` nickName
            case result of 
                Nothing -> do 
                        liftIO $ WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                Just _ -> do 
                    liftIO $ putStrLn $ "Writer thread :-> Message nickName " `mappend` (show nickName)
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


getPortNumberR :: Handler Html 
getPortNumberR = do 
    defaultLayout $ do 
        [whamlet| 3000 |]
getHomeR :: Handler Html
getHomeR = do
    request <- waiRequest
    liftIO $ putStrLn $ "Request " ++ (show request)
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
    hSetBuffering stdout NoBuffering
    connStr <- getConnectionString
    poolSize <- getPoolSize
    runStderrLoggingT $ withPostgresqlPool connStr poolSize $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                runMigration migrateAll
    chan <- atomically newBroadcastTChan
    static@(Static settings) <- static "static"
    nickNameMap <- newTVarIO $ IMap.empty
    warp 3000 $ App chan static nickNameMap

instance ToJSON LoginStatus
instance FromJSON LoginStatus