{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module CCAR.Main.Driver
    (driver)
where 

import Yesod.Core
import Yesod.WebSockets as YWS
import Control.Monad.Trans.Control    (MonadBaseControl (liftBaseWith, restoreM))
import Network.WebSockets.Connection as WSConn
import Yesod.Static
import qualified GHC.Conc as GHCConc
import qualified Data.Text.Lazy as TL
import CCAR.Parser.CCARParsec
import Control.Monad (forever, void, when)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async as A (waitSTM, wait, async, cancel)
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


connStr = "host=localhost dbname=ccar_debug user=ccar password=ccar port=5432"

emptyPerson = Person {personFirstName = "Not known"
                    , personLastName = "Not known"
                    , personNickName = "undefined"
                    , personPassword = "Not known"
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
data ErrorCommand = ErrorCommand {errorCode :: T.Text, message :: T.Text} 
                deriving (Show, Eq)

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

instance ToJSON CCARText where 
    toJSON (CCARText u s cc) = object ["textUploadedBy" .= u 
                                        , "scenarioName" .= s
                                        , "ccarText" .= (readExpr cc)
                                        , "commandType" .= ("ParsedCCARText" :: T.Text)]

type KeepAliveCommand = T.Text
data Command = CommandLogin Login 
                | CommandUO UserOperations 
                | CommandUTO UserTermsOperations
                | CommandCCARUpload CCARUpload
                | CommandError ErrorCommand 
                | CommandKeepAlive KeepAliveCommand
                | ParseCCARText CCARText
                deriving(Show, Eq)

data UserPreferences = UserPreferences {prefs :: T.Text} deriving (Show, Eq, Generic)


genPerson (Person a b c d e) = object ["firstName" .= a
                                       , "lastName" .= b
                                       , "nickName" .= c
                                       , "password" .= d
                                       , "deleted" .= e]

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
genErrorCommand (ErrorCommand e  m) = object ["errorCode" .= e
                                              , "message" .= m]

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
instance ToJSON ErrorCommand where
    toJSON = genErrorCommand

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

genericErrorCommand errorMessage = ErrorCommand {errorCode = T.pack "Error" 
                                       , message = T.pack errorMessage}
parseErrorCommand value= do
        return $ ErrorCommand {errorCode = T.pack "Error"
                               , message = T.pack $ show value}


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

parsePerson v = Person <$>
                    v .: "firstName" <*>
                    v .: "lastName" <*>
                    v .: "nickName" <*>
                    v .: "password"  <*>
                    v .: "deleted"



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
 
decoder = L.toStrict . E.decodeUtf8 . En.encode        
decoderM a = return $ L.toStrict $ E.decodeUtf8 $ En.encode a



type NickName = T.Text
type ClientMap = GroupCommunication.ClientIdentifierMap

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
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
            liftIO $ do
                flip runSqlPersistMPool pool $ do 
                        cid <- DB.insert c
                        $(logInfo) $ T.pack $ show ("Returning " ++ (show cid))
                        return cid

updateCCAR :: CCAR -> IO (Maybe CCAR)
updateCCAR c = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
            flip runSqlPersistMPool pool $ do
                DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARScenarioText =. (cCARScenarioText c)]
                return $ Just c 

queryAllCCAR :: T.Text -> IO [Entity CCAR]
queryAllCCAR aNickName = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    selectList [] []
 
queryCCAR :: CCARId -> IO (Maybe CCAR) 
queryCCAR pid = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    get pid 

deleteCCAR :: CCAR -> IO (Maybe CCAR)
deleteCCAR c = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
            flip runSqlPersistMPool pool $ do
                DB.updateWhere [CCARScenarioName ==. (cCARScenarioName c)] [CCARDeleted =. True]
                return $ Just $ c {cCARDeleted = True} 

checkLoginExists :: T.Text  -> IO (Maybe (Entity Person))
checkLoginExists aNickName = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
        liftIO $ do
            flip runSqlPersistMPool pool $ do
                getBy $ PersonUniqueNickName aNickName

insertPerson :: Person -> IO ((Key Person)) 
insertPerson p = do 
        putStrLn $ show $ "Inside insert person " ++ (show p)
        runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
            liftIO $ do
                flip runSqlPersistMPool pool $ do 
                        pid <- DB.insert p
                        $(logInfo) $ T.pack $ show  ("Returning " ++ (show pid))
                        return pid

updatePerson :: PersonId -> Person -> IO (Maybe Person)
updatePerson pid p = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
            flip runSqlPersistMPool pool $ do
                DB.replace (pid) p
                get pid

queryPerson :: PersonId -> IO (Maybe Person) 
queryPerson pid = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
            liftIO $ do 
                flip runSqlPersistMPool pool $ 
                    get pid 

deletePerson :: PersonId -> Person -> IO (Maybe Person)
deletePerson pid p = updatePerson pid p {personDeleted = True}

{- Read the message, parse and then send it back. -}
processCommand :: Maybe Command  -> IO T.Text
processCommand (Just (CommandLogin aLogin)) = do 
    chk <- checkLoginExists (personNickName p)
    putStrLn $ show $ "Login exists " ++ (show chk)
    case chk of 
        Nothing -> return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandLogin $ Login {login = Just p, 
            loginStatus = Just UserNotFound}
        Just (Entity aid a)  -> 
            return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandLogin $ 
                Login {login = Just a, loginStatus = Just UserExists}
    where
        p = case (login aLogin) of
                Nothing -> emptyPerson
                Just a -> a 

processCommand (Just (CommandError error)) = 
        return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandError error
processCommand Nothing = return 
         $ decoder $ genericErrorCommand "Unable to process command"

processCommand (Just (CommandKeepAlive a)) = 
        return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandKeepAlive a 

processCommand (Just ( CommandUO (UserOperations uo aPerson))) = do
    putStrLn $ show $ "Processing processCommand " ++ (show uo)
    case uo of
        Us.Create  -> do
                personId <- insertPerson person
                putStrLn $ show $ "Person inserted " ++ (show personId)
                return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandUO 
                        $ UserOperations Us.Create (Just person)
        Us.Update personId -> do
                updatePerson personId person
                return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandUO 
                            $ UserOperations (Us.Update personId) (Just person)
        Us.Delete personId -> do 
                deletePerson personId person
                return $ L.toStrict $ E.decodeUtf8 $ J.encode $ 
                        CommandUO $ UserOperations (Us.Delete personId) (Just person)
        Us.Query personId -> do 
                maybePerson <- queryPerson (personId)
                case maybePerson of
                    Nothing -> 
                        return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandUO  
                                $ UserOperations (Us.Query personId) Nothing
                    Just (p) -> 
                        return $ L.toStrict $ E.decodeUtf8 $ J.encode $ CommandUO  
                                $ UserOperations (Us.Query personId) (Just p)
    where
        person = case aPerson of
                Nothing -> emptyPerson
                Just a -> a        


processCommand (Just (CommandCCARUpload (CCARUpload nickName operation aCCAR aList))) = do
    putStrLn $ show $ "Processing command ccar upload " ++ (show ccar)
    case operation of 
        CC.Create -> do 
                ccarId <- insertCCAR ccar
                putStrLn $ show $ "CCAR created " ++ (show ccar)
                return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName operation 
                                (Just ccar) []
        CC.Update  -> do
                updateCCAR ccar 
                return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName operation (Just ccar) []
        CC.Delete  -> do 
                res <- deleteCCAR ccar 
                return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName operation (res) []
        CC.Query ccarId -> do 
                maybeCCAR <- queryCCAR (ccarId)
                case maybeCCAR of 
                    Nothing -> return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName (CC.Query ccarId) Nothing []
                    Just x -> return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName (CC.Query ccarId) (Just x) []
        CC.QueryAll nickName -> do
                maybeCCAR <- queryAllCCAR nickName
                case maybeCCAR of 
                    [] -> return $ L.toStrict $ E.decodeUtf8 $ J.encode 
                        $ CommandCCARUpload $ CCARUpload nickName (CC.QueryAll nickName) Nothing []
                    aList -> return $ L.toStrict $ E.decodeUtf8 $ J.encode $ 
                            CommandCCARUpload $ CCARUpload nickName (CC.QueryAll nickName) Nothing (ccarList aList)
                    where
                        ccarList aList = Prelude.map( \(Entity y  x) -> Just x) aList
        where
            ccar = case aCCAR of
                    Nothing -> emptyCCAR
                    Just a -> a

processCommand (Just a) = return $ L.toStrict $ E.decodeUtf8 
                                $ J.encode $ a

processCommandWrapper :: Value -> Command 
processCommandWrapper (Object a)   = 
    case cType of 
        Nothing -> CommandError $ genericErrorCommand "Unable to process command"
        Just aType -> 
            case aType of 
                String "Login" -> 
                        case (parse parseLogin a) of
                            Success r -> CommandLogin r
                            Error s -> CommandError $ genericErrorCommand $ "parse login  failed "++ s
                String "ManageUser" ->
                        case (parse parseCreateUser a) of
                            Success r -> CommandUO r
                            Error s -> CommandError $ genericErrorCommand $ "parse manage user failed " ++ s
                String "CCARUpload" ->
                        case (parse parseCCARUpload a) of
                            Success r -> CommandCCARUpload r 
                            Error s -> CommandError $ genericErrorCommand $ "parse ccar upload failed " ++ s ++ (show a)
                String "KeepAlive" ->
                        case (parse parseKeepAlive a) of
                            Success r -> CommandKeepAlive r 
                            Error s -> CommandError $ genericErrorCommand $ "Parse Keep alive failed" ++ s ++ (show a)
                String "ParsedCCARText" ->
                        case (parse parseCCARText a) of
                            Success r -> ParseCCARText r 
                            Error s -> CommandError $ genericErrorCommand $ "Parse CCAR Text " ++ s ++ (show a)
                _ -> CommandError $ genericErrorCommand ("Unable to process command " ++ (show aType))
    where 
        cType =  LH.lookup "commandType" a


processCommandWrapper _ = CommandError $ genericErrorCommand "Not yet implemented"

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
                            String x -> return (Just "found it", x)
                            _ -> return (Nothing, T.pack $ "Invalid " ++ (show x))
                where 
                    nn = LH.lookup "nickName" a 

processIncomingMessage :: Maybe Value -> IO T.Text
processIncomingMessage aCommand = 
    do 
        putStrLn $  "Processing incoming message " ++ (show aCommand)
        case aCommand of 
            Nothing -> do
                    putStrLn $ "Processing error..."
                    return $ L.toStrict $ E.decodeUtf8 $ En.encode   
                        (CommandError $ genericErrorCommand ("Unknown error"))
            Just (Object a) -> do 
                    putStrLn $ show $ "Processing command type " ++ (show (LH.lookup "commandType" a))
                    c <- return $ processCommandWrapper (Object a)
                    putStrLn $ show $ "Processing command " ++ (show c)
                    reply <- processCommand (Just c)
                    return $ L.toStrict $ E.decodeUtf8 $ En.encode reply



sendPrivateMessage :: App -> Login -> Login -> IO ()
sendPrivateMessage app source destination = undefined 
sendMessage :: App -> Login -> [Login] -> IO ()
sendMessage app source destinations = undefined


deleteConnection :: App -> WSConn.Connection -> Login -> IO  () 
deleteConnection app conn lo@(Login p status) = do
    case p of
        Just x -> atomically $ do 
                cMap <- readTVar $ nickNameMap app                
                _ <- writeTVar (nickNameMap app) (IMap.delete (personNickName x) cMap)
                return ()
        Nothing -> return ()

addConnection :: App -> WSConn.Connection ->  Login -> IO ()
addConnection app aConn lo@(Login p status) =do
    case p of
        Just x -> atomically $ do 
                nMap <- readTVar $ nickNameMap app 
                w <- newTChan
                r <- dupTChan w 
                clientState <- return ClientState{nickName = (personNickName x)
                        , connection = aConn
                        , readChan = r 
                        , writeChan = w 
                }
                _ <- writeTVar (nickNameMap app) (IMap.insert (personNickName x) clientState nMap)
                return ()
        Nothing -> return ()

getClientState :: T.Text -> App -> STM ClientState
getClientState nickName app@(App a b c) = do
        nMap <- readTVar c
        return $ nMap ! nickName

handleInitialLogin :: WSConn.Connection -> T.Text -> App -> IO Command
handleInitialLogin aConn aText app@(App a b c) = 
        do 
            case aCommand of 
                Nothing -> return $ CommandError $ genericErrorCommand ("Login has errors")
                Just (Object a) -> do
                    c <- return $ processCommandWrapper(Object a) 
                    case c of 
                        (CommandLogin r) -> do 
                                addConnection app aConn r
                                return c 
                        _ -> return $ CommandError $ genericErrorCommand ("Invalid command during login")
            where 
                aCommand = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value


instance Show WSConn.Connection where
    show (WSConn.Connection o cType proto msgIn msgOut cl) = show proto 

ccarApp :: WebSocketsT Handler ()
ccarApp = do
        connection <- ask
        app <- getYesod
        command <- YWS.receiveData
        _ <- liftIO $ handleInitialLogin connection command app  
        $(logInfo) $ T.pack $ show $ "Connection " `mappend` (show connection)
        $(logInfo) $ "Incoming text " `mappend` (command :: T.Text)
        $(logInfo) $ T.pack $ show $ incomingDictionary (command :: T.Text)
        -- The nick name at startup
        -- The commands should always have a nickName for the source of the cohmands.

        (result, nickNameStartup) <- liftIO $ getNickName $ incomingDictionary command
        case result of 
                Nothing -> 
                    do
                        liftIO $ WSConn.sendClose connection ("Nick name tag is mandatory. Bye" :: T.Text)
                Just x -> do
                        nickNameExists <- liftIO $ processIncomingMessage $ incomingDictionary command
                        $(logInfo) nickNameExists
                        YWS.sendTextData nickNameExists
                        a <- liftBaseWith (\run -> A.async $ run writer)
                        b <- liftBaseWith (\run -> A.async $ run $ liftIO $ reader app nickNameStartup)
                        liftBaseWith (\run -> A.wait a)
                        liftBaseWith (\run -> A.wait b)
                        $(logInfo) "Thread exiting"
        where
            incomingDictionary aText = (J.decode  $ E.encodeUtf8 (L.fromStrict aText)) :: Maybe Value
            
            -- Writer writes to the write channel 
            -- with source being the websocket
            writer = do
                app <- getYesod
                msg <- YWS.receiveData
                (_, nickNameLatest) <- liftIO $ getNickName $ incomingDictionary msg
                liftIO $ putStrLn $ "Latest nickName " `mappend` (show nickNameLatest)
                x <- liftIO $ processIncomingMessage $ incomingDictionary msg
                atomically $ do 
                            clientState <- getClientState nickNameLatest app
                            writeTChan (writeChan clientState) x
                writer
            reader app nickN= do
                putStrLn "Waiting for messages..."
                
                (connection, textData) <- atomically $ do
                        clientState <- getClientState nickN app        
                        textData <- readTChan (readChan clientState)                        
                        return (connection clientState, textData)
                --putStrLn $ "Sending text data " `mappend` (T.unpack textData)
                WSConn.sendTextData (connection) textData    
                liftIO $ putStrLn $ "Wrote " `mappend` (show textData)
                reader app nickN

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
