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
import CCAR.Main.DBUtils
import GHC.Generics
import Data.Data
import Data.Typeable 
import Database.Persist.Postgresql as DB
import Data.Map as IMap
import CCAR.Main.GroupCommunication as GroupCommunication
import CCAR.Main.UserJoined as UserJoined 
import Data.ByteString as DBS hiding (putStrLn)
import Data.ByteString.Char8 as C8 hiding(putStrLn) 
import System.Environment
import CCAR.Command.ErrorCommand 

--connStr = "host=localhost dbname=ccar_debug user=ccar password=ccar port=5432"
connStr = getConnectionString

emptyPerson :: IO Person 
emptyPerson = do 
                time <- getCurrentTime
                return $ Person {personFirstName = "Not known"
                    , personLastName = "Not known"
                    , personNickName = "undefined"
                    , personPassword = "Not known"
                    , personLastLoginTime = time 
                    , personDeleted = False}



emptyCCAR = CCAR {
        cCARScenarioName =  "Not known"
        , cCARScenarioText = "Not known"
        , cCARCreator = "Not Known"
        , cCARDeleted = False
}
data LoginStatus = UserExists | UserNotFound | InvalidPassword | Undefined
    deriving(Show, Typeable, Data, Generic, Eq)

{- List of commands that we support.-}

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
                                       , "lastLoginTime" .= e
                                       , "deleted" .= f]

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
genCommandKeepAlive a  = object ["keepAlive" .= a
                                , "commandType" .= ("keepAlive" :: T.Text)]


instance ToJSON Person where
    toJSON  = genPerson 

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

instance FromJSON Person where
    parseJSON (Object v) = parsePerson v
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
    putStrLn $  "pJSON " ++ (T.unpack aText)
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
/ HomeR GET
/static StaticR  Static getStatic
|]


insertCCAR :: CCAR -> IO (Key CCAR) 
insertCCAR c = do 
        putStrLn $ "Inside insert ccar"
        cStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool cStr 10 $ \pool -> 
            liftIO $ do
                flip runSqlPersistMPool pool $ do 
                        cid <- DB.insert c
                        $(logInfo) $ T.pack $ show ("Returning " ++ (show cid))
                        return cid

updateCCAR :: CCAR -> IO (Maybe CCAR)
updateCCAR c = do 
        cStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool cStr 10 $ \pool ->
            liftIO $ do
                flip runSqlPersistMPool pool $ do
                    DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARScenarioText =. (cCARScenarioText c)]
                    return $ Just c 

queryAllCCAR :: T.Text -> IO [Entity CCAR]
queryAllCCAR aNickName = do 
        cStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool cStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    selectList [] []
 
queryCCAR :: CCARId -> IO (Maybe CCAR) 
queryCCAR pid = do 
        connStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    get pid 

deleteCCAR :: CCAR -> IO (Maybe CCAR)
deleteCCAR c = do 
        connStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do
                flip runSqlPersistMPool pool $ do
                    DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARDeleted =. True]
                    return $ Just $ c {cCARDeleted = True} 

updateLogin :: Person -> IO (Maybe Person) 
updateLogin p = do
        connStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
                liftIO $ do 
                    now <- getCurrentTime
                    flip runSqlPersistMPool pool $ do 
                        DB.updateWhere [PersonNickName ==. (personNickName p)][PersonLastLoginTime =. now] 
                    return $ Just p 
checkLoginExists :: T.Text  -> IO (Maybe (Entity Person))
checkLoginExists aNickName = do 
    connStr <- getConnectionString
    runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                getBy $ PersonUniqueNickName aNickName

insertPerson :: Person -> IO ((Key Person)) 
insertPerson p = do 
        putStrLn $ show $ "Inside insert person " ++ (show p)
        connStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
            liftIO $ do
                flip runSqlPersistMPool pool $ do 
                        pid <- DB.insert p
                        $(logInfo) $ T.pack $ show  ("Returning " ++ (show pid))
                        return pid

updatePerson :: PersonId -> Person -> IO (Maybe Person)
updatePerson pid p = do 
    connStr <- getConnectionString
    runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                DB.replace (pid) p
                get pid

queryPerson :: PersonId -> IO (Maybe Person) 
queryPerson pid = do
        connStr <- getConnectionString
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    get pid 

deletePerson :: PersonId -> Person -> IO (Maybe Person)
deletePerson pid p = updatePerson pid p {personDeleted = True}


processCommand :: Maybe Command  -> IO (DestinationType, Command)
processCommand (Just (CommandLogin aLogin)) = do 
    p <- case (login aLogin) of
            Nothing -> emptyPerson
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
                Nothing -> emptyPerson
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
        CC.Query ccarId -> do 
                maybeCCAR <- queryCCAR (ccarId)
                case maybeCCAR of 
                    Nothing -> return $ (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName (CC.Query ccarId) Nothing [])
                    Just x -> return $  (GroupCommunication.Reply, CommandCCARUpload $ CCARUpload nickName (CC.Query ccarId) (Just x) []) 
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
                    Nothing -> emptyCCAR
                    Just a -> a


processCommand (Just a) = return (GroupCommunication.Reply, a)

processCommandWrapper :: Value -> IO (DestinationType, T.Text)
processCommandWrapper (Object a)   = do  
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
                            Success r -> do
                                    (d, c) <- processCommand $ Just $ CommandUO r
                                    return (d, ser c)
                            Error s -> 
                                return (GroupCommunication.Reply 
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
                _ -> 
                    return 
                         ( GroupCommunication.Reply
                         , ser $ CommandError $ genericErrorCommand ("Unable to process command " ++ (show aType)))
    where 
        cType =  LH.lookup "commandType" a
        ser a = L.toStrict $ E.decodeUtf8 $ En.encode a 


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

processIncomingMessage :: Maybe Value -> IO (DestinationType , T.Text)
processIncomingMessage aCommand = 
    do 
        putStrLn $  "Processing incoming message " ++ (show aCommand)
        case aCommand of 
            Nothing -> do
                    putStrLn $ "Processing error..."
                    result <- return (CommandError $ genericErrorCommand ("Unknown error")) 
                    return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode  result)
                        
            Just (Object a) -> do 
                    putStrLn $ show $ "Processing command type " ++ (show (LH.lookup "commandType" a))
                    processCommandWrapper (Object a)
                    --return $ (d, L.toStrict $ E.decodeUtf8 $ En.encode command)
                    


deleteConnection :: App -> WSConn.Connection -> T.Text -> STM  () 
deleteConnection app conn nn = do 
            cMap <- readTVar $ nickNameMap app                
            _ <-    writeTVar (nickNameMap app) (IMap.delete nn cMap)
            return ()

addConnection :: App -> WSConn.Connection ->  T.Text -> IO ()
addConnection app aConn nn = atomically $ do 
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
        return $ [nMap ! nickName]

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
                            L.toStrict $ E.decodeUtf8 $ En.encode $ CommandError $ genericErrorCommand ("Login has errors"))
                Just (Object a) -> do
                    (d, c) <- processCommandWrapper(Object a) 
                    c1 <- return $ (J.decode $ E.encodeUtf8 $ L.fromStrict c :: Maybe Value)
                    case c1 of 
                        Just (Object a ) -> do  
                            result <- return $ parse parseLogin a 
                            case result of 
                                Success (r@(Login a b)) -> do 
                                        nickName <- getPersonNickName a 
                                        userJoined <- return $ UserJoined.userJoined nickName 
                                        return (GroupCommunication.Reply, userJoined)
                                _ -> return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode 
                                                    $ CommandError $ genericErrorCommand ("Invalid command during login"))
                        _ -> return $ (GroupCommunication.Reply, L.toStrict $ E.decodeUtf8 $ En.encode 
                                                    $ CommandError $ genericErrorCommand ("Invalid command during login"))
            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value


processUserLoggedIn :: WSConn.Connection -> T.Text -> App -> IO (DestinationType, T.Text) 
processUserLoggedIn aConn aText app@(App a b c) = 
    do
        case aCommand of 
                Nothing -> return (GroupCommunication.Reply, 
                        ser $ CommandError $ genericErrorCommand ("Login has errors"))
                Just (Object a) -> do
                    c <- return $ parse UserJoined.parseUserLoggedIn a  
                    case c of 
                        Success u@(UserJoined.UserLoggedIn a) -> do 
                                addConnection app aConn a 
                                return $ (Broadcast, ser u)
                        _ -> return $ (GroupCommunication.Reply, ser  
                                            $ CommandError $ genericErrorCommand ("Invalid command during login"))
            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value
                ser  = (L.toStrict) . (E.decodeUtf8) . (En.encode)

instance Show WSConn.Connection where
    show (WSConn.Connection o cType proto msgIn msgOut cl) = show proto 

ccarApp :: WebSocketsT Handler ()
ccarApp = do
        connection <- ask
        app <- getYesod
        command <- YWS.receiveData
        (destination, text) <- liftIO $ authenticate connection command app  


        liftIO $ putStrLn $ "Testing authenticate " `mappend` show (text :: T.Text)
        $(logInfo) $ T.pack $ show $ "Connection " `mappend` (show connection)
        $(logInfo) $ "Incoming text " `mappend` (command :: T.Text)
        $(logInfo) $ T.pack $ show $ incomingDictionary (command :: T.Text)

        (result, nickNameV) <- liftIO $ getNickName $ incomingDictionary command
        case result of 
                Nothing -> liftIO $  
                            WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text) >>
                            return ()
                Just _ -> liftIO $ 
                            (processNoException app connection nickNameV command)
                             `catch` 
                                    (\ a@(CloseRequest e1 e2) -> do  
                                        atomically $ deleteConnection app connection nickNameV
                                        return ())


incomingDictionary aText = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value


processNoException app connection nickNameV iText = do
                    (command, nickNameFound) <- liftIO $ processIncomingMessage $ incomingDictionary iText
                    WSConn.sendTextData connection nickNameFound
                    command <- (processInnerException connection app nickNameV ) `catch` 
                                                (\ (CloseRequest e1 e2) -> do                                                             
                                                        return (UserJoined.userLeft nickNameV)
                                                    )
                    return ()
    

processInnerException connection app nickNameV = do
            command <- WSConn.receiveData connection
            (dest, text) <- liftIO $ processUserLoggedIn connection command app 
            messageHistory <- liftIO $ GroupCommunication.getMessageHistory 1000 -- read from db.
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
                            -- Now send the list of users to the current connection
                            -- send it to the current connection
                            -- XXX: Need to clean this up
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
            a <- liftBaseWith (\run -> run $ liftIO $ do
                                        a <- (A.async (writerThread app connection 
                                            nickNameV False))
                                        b <- (A.async (liftIO $ readerThread app nickNameV))
                                        A.waitEither a b
                                        return "Threads had exception")
            return "Threads Exiting"

handleDisconnects :: App -> WSConn.Connection -> T.Text -> ConnectionException -> IO ()
handleDisconnects app connection nickN (CloseRequest a b) = do 
            putStrLn $ T.unpack $  
                ("Bye nickName " :: T.Text) `mappend` nickN
            atomically $ do 
                deleteConnection app connection nickN
                restOfUs <- getAllClients app nickN 
                mapM_ (\cs -> 
                        writeTChan(writeChan cs) $ 
                            UserJoined.userLeft nickN )restOfUs

            
readerThread app nickN= do
    putStrLn "Waiting for messages..."
    (connection, textData) <- atomically $ do
            clientState : _ <- getClientState nickN app        
            textData <- readTChan (readChan clientState)                        
            return (connection clientState, textData)
    --putStrLn $ "Sending text data " `mappend` (T.unpack textData)
    WSConn.sendTextData (connection) textData `catch` 
        (\h@(CloseRequest e f)-> handleDisconnects app connection nickN h)
    liftIO $ putStrLn $ "Wrote " `mappend` (show textData) `mappend` (show connection)
    readerThread app nickN

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
            (result, nickName) <- liftIO $ getNickName $ incomingDictionary msg
            case result of 
                Nothing -> liftIO $ WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                Just _ -> do 
                    liftIO $ putStrLn $ "Writer thread :-> Message nickName " `mappend` (show nickName)
                    (dest, x) <- liftIO $ processIncomingMessage $ incomingDictionary msg
                    liftIO $ putStrLn $ "Writer thread :-> Writing command " ++ (show x)
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

            url = url.`("http:", "ws:").replace("https:", "wss:");
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
    runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                runMigration migrateAll
    chan <- atomically newBroadcastTChan
    static@(Static settings) <- static "static"
    nickNameMap <- newTVarIO $ IMap.empty
    warp 3000 $ App chan static nickNameMap

instance ToJSON LoginStatus
instance FromJSON LoginStatus
