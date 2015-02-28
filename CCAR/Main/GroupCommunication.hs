{-# LANGUAGE RecordWildCards #-}

module CCAR.Main.GroupCommunication where

import Control.Concurrent
import Control.Concurrent.STM.Lifted
import Control.Concurrent.Async
import qualified  Data.Map as IMap
import Control.Exception
import Control.Monad
import Network.WebSockets.Connection as WSConn
import Data.Text as T


{- 
	This websocket client needs to handle different kinds of messages that can broadly classified as
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

