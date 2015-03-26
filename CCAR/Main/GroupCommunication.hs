{-# LANGUAGE RecordWildCards #-}

module CCAR.Main.GroupCommunication 
	(ClientState(..)
	, ClientIdentifierMap(..)
	, processSendMessage
    , getMessageHistory
	, DestinationType(..) )
where
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
import Data.Time(UTCTime, getCurrentTime)
import GHC.Generics
import Data.Data
import Data.Typeable 
import CCAR.Main.DBUtils
import CCAR.Main.EnumeratedTypes as Et
import CCAR.Command.ErrorCommand


{- 
	The client needs to handle
		. Broadcast 
		- Group broadcast (members can join and leave the group)
		- Private messages (members can send private messages to the group)
		- Response messages (client requests and the server responds with a reply)
	The client needs to handle async concurrent exceptions and mask them as mentioned in
	Marlowe book.
	Following the model in the above book, we can assume that each client spawns 4 threads 
	to write to and a corresponding read channel for each connection to do the write.
-}

{-The server state is represented as -}

type ClientIdentifier = T.Text
data ClientState = ClientState {
			nickName :: ClientIdentifier
			, connection :: WSConn.Connection
			, readChan :: TChan T.Text
			, writeChan :: TChan T.Text
	}
type ClientIdentifierMap = TVar (IMap.Map ClientIdentifier ClientState)
type GroupIdentifier = T.Text
data DestinationType = Reply | GroupMessage GroupIdentifier | Broadcast | PrivateMessage ClientIdentifier
		deriving(Show, Typeable, Data, Generic, Eq)

data SendMessage = SendMessage { from :: T.Text
                                , to :: T.Text
                                , privateMessage ::  T.Text
                                , destination :: DestinationType
                                , sentTime :: UTCTime } deriving (Show, Eq)

createBroadcastMessage :: MessageP -> Maybe SendMessage 
createBroadcastMessage (MessageP fr to pM _ Et.Broadcast currentTime) = Just $ 
        SendMessage fr to pM CCAR.Main.GroupCommunication.Broadcast currentTime
createBroadcastMessage (MessageP fr to pM _ _ aTime)            = Nothing 

createPersistentMessage :: SendMessage -> MessageP 
createPersistentMessage cm@(SendMessage fr to pM destination currentTime) = do
		case destination of 
			CCAR.Main.GroupCommunication.Reply -> 
					MessageP fr to pM Et.Undecided Et.Reply currentTime 
			_ 	  -> 
					MessageP fr to pM Et.Undecided Et.Broadcast currentTime

getAllMessages :: Int -> IO [Entity MessageP]
getAllMessages limit = dbOps $ selectList [] [Asc MessagePSentTime]

saveMessage :: SendMessage -> IO (Key MessageP) 
saveMessage c = dbOps $ do 
                do 
                    cid <- DB.insert $ createPersistentMessage c 
                    $(logInfo) $ T.pack $ show ("Returning " ++ (show cid))
                    return cid

getMessageHistory :: Int -> IO [T.Text]
getMessageHistory limit = do
    allM <- getAllMessages limit
    messages <- mapM (\(Entity y x) -> do 
                            m <- return $ createBroadcastMessage x
                            case m of
                                Just m1 -> return $ serialize m1
                                Nothing -> return "") allM
    return messages


process (cm@(SendMessage f t m d time)) = do

    case d of 
        CCAR.Main.GroupCommunication.Broadcast -> do 
        	saveMessage cm 
        	return (CCAR.Main.GroupCommunication.Broadcast, serialize cm)
        _ -> return (CCAR.Main.GroupCommunication.Reply, serialize cm) 





genSendMessage (SendMessage f t m d sT) = object ["from" .= f
                    , "to" .= t
                    , "privateMessage" .= m
                    , "commandType" .= ("SendMessage" :: T.Text)
                    , "destination" .= d
                    , "sentTime" .= sT]
parseSendMessage v = SendMessage <$> 
                    v .: "from" <*>
                    v .: "to" <*>
                    v .: "privateMessage" <*>
                    v .: "destination" <*>
                    v .: "sentTime"


processSendMessage (Object a) = 
        case (parse parseSendMessage a) of
            Success r ->  process r 
            Error s -> return (CCAR.Main.GroupCommunication.Reply, 
            			serialize $ genericErrorCommand $ "Sending message failed " ++ s ++ (show a))


instance ToJSON DestinationType
instance FromJSON DestinationType
instance ToJSON SendMessage where
    toJSON (SendMessage f t m d time) = genSendMessage (SendMessage f t m d time)

instance FromJSON SendMessage where 
    parseJSON (Object v ) = parseSendMessage v 
    parseJSON _           = Appl.empty

serialize :: (ToJSON a) => a -> T.Text 
serialize a = L.toStrict $ E.decodeUtf8 $ En.encode a 
