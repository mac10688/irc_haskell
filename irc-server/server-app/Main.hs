{-# LANGUAGE OverloadedStrings #-}
{-
websockets example
==================

This is the Haskell implementation of the example for the WebSockets library. We
implement a simple multi-user chat program. A live demo of the example is
available [here](/example/client.html).  In order to understand this example,
keep the [reference](/reference/) nearby to check out the functions we use.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as BM
import Payload
import Data.Aeson
import Data.UUID.V4 as 

import qualified Network.WebSockets as WS

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.

--type Client = BiMap Text, WS.Connection

-- The state kept on the server is simply a list of connected clients. We've added
-- an alias and some utility functions, so it will be easier to extend this state
-- later on.

data Room = Room [Text]

data ServerState = ServerState {
    clients :: BM.Map Text WS.Connection,
    rooms :: BM.Map UUID Room
}

-- Create a new, initial state:

newServerState :: ServerState
newServerState = ServerState BM.empty

-- Get the number of active clients:

numClients :: ServerState -> Int
numClients = BM.size . clients

-- Check if a user already exists (based on username):

clientExists :: Text -> ServerState -> Bool
clientExists name = BM.member name . clients

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: Text -> WS.Connection -> ServerState -> ServerState
addClient name conn  = ServerState . BM.insert name conn . clients

-- Remove a client:

removeClient :: Text -> ServerState -> ServerState
removeClient name = ServerState . BM.delete name . clients

createUUID :: IO UUID
createUUID = nextRandom

createRoom :: UUID -> Text -> ServerState -> ServerState
createRoom 

-- Send a message to all clients, and log it on stdout:

broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    T.putStrLn message
    forM_ (BM.elems (clients state)) $ \conn -> WS.sendTextData conn message

-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

main :: IO ()
main = do
    putStrLn "Starting server"
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

-- Our main application has the type:

application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        clients' <- readMVar state
        lname <- return $ loginName <$> (decode msg :: Maybe Connect)
        case lname of
            Just name   | any ($ name)
                            [T.null, T.any isPunctuation, T.any isSpace] ->
                                WS.sendClose conn ("Name cannot " <>
                                    "contain punctuation or whitespace, and " <>
                                    "cannot be empty" :: Text)
                        | clientExists name clients' ->
                            WS.sendClose conn ("User already exists" :: Text)
                        | otherwise -> 
                            flip finally (disconnect name) $ do
                            modifyMVar_ state $ \s -> do
                                let s' = addClient name conn s
                                -- WS.sendTextData conn $
                                --      "Welcome! Users: " <>
                                --      T.intercalate ", " (BM.map fst s)
                                broadcast (name <> " joined") s'
                                return s'
                            talk name conn state
            Nothing -> WS.sendClose conn ("Not working" :: Text) --test that this actually closes a connection
            where
            disconnect name = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient name s in return (s', s')
                broadcast (name <> (" disconnected" :: Text)) s

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.
talk :: Text -> WS.Connection -> MVar ServerState -> IO ()
talk user conn state = forever $ do
    msg <- WS.receiveData conn
    actionM <- return $ (decode msg :: Maybe Action)
    case actionM of
        Just (action) -> case action of
                                CreateRoom name -> modifyMVar_ state $ \s -> do
                                    let s' = createRoom name s
                                    WS.sendTextData 
                                JoinRoom id -> readMVar state >>= broadcast "Joining a room"
                                LeaveRoom id -> readMVar state >>= broadcast "Leaving a room"
                                ListAllRooms -> readMVar state >>= broadcast "Listing all rooms"
                                ListRoomMembers roomId -> readMVar state >>= broadcast "Listing all room members"
                                SendMsgRoom id msg -> readMVar state >>= broadcast "Sending the room a message"
        Nothing -> WS.sendTextData conn $ ("Error" :: Text)

    -- readMVar state >>= broadcast
    --     (user `mappend` ": " `mappend` msg)

    {-
    data Action 
    | CreateRoom Text
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | SendMsgRoom UUID Text
    deriving (Generic, Show)
    -}