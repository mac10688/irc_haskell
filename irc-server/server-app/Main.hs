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
import qualified Data.Map as M
import qualified Data.Set as S
import Payload
import Data.Aeson
import Data.UUID
import Data.UUID.V4
import Data.Set as S
import Data.Maybe
import Data.Monoid

import qualified Network.WebSockets as WS

data Room = Room {
    roomId :: UUID,
    roomName :: Text
}

data User = User {
    userId :: UUID,
    userName :: Text,
    connection :: WS.Connection
}

data ServerState = ServerState {
    users :: M.Map UUID User,
    rooms :: M.Map UUID Room,
    userToRoomMappings :: M.Map UUID (S.Set UUID),
    roomToUserMappings :: M.Map UUID (S.Set UUID),
    usedUserNames :: S.Set Text,
    usedRoomNames :: S.Set Text
}

newServerState :: ServerState
newServerState = ServerState {
    users = M.empty,
    rooms = M.empty,
    userToRoomMappings = M.empty,
    roomToUserMappings = M.empty,
    usedUserNames = S.empty,
    usedRoomNames = S.empty
}

nameTaken :: Text -> ServerState -> Bool
nameTaken name = S.member name . usedUserNames

addUser :: UUID -> Text -> WS.Connection -> ServerState -> ServerState
addUser id name conn state = 
    let
        users' = M.insert id (User id name conn) $ users state 
        usedUserNames' = S.insert name $ usedUserNames state
        userToRoomMappings' = M.insert id S.empty $ userToRoomMappings state
    in state { 
        users = users',
        usedUserNames = usedUserNames',
        userToRoomMappings = userToRoomMappings'
    }

removeUser :: UUID -> ServerState -> ServerState
removeUser userId state =
    let 
        possibleUserName = fromMaybe mempty $ userName <$> (M.lookup userId $ users state)
        users' = M.delete userId $ users state
        usedUserNames' = S.delete possibleUserName $ usedUserNames state
        roomsToRemoveTheUser = fromMaybe S.empty $ M.lookup userId $ userToRoomMappings state
        roomToUserMappings' = 
            S.foldr (\roomId map -> M.update (\set -> Just $ S.delete userId set) roomId map ) (roomToUserMappings state) roomsToRemoveTheUser
        userToRoomMappings' = M.delete userId $ userToRoomMappings state
    in state {
        users = users',
        usedUserNames = usedUserNames',
        userToRoomMappings = userToRoomMappings',
        roomToUserMappings = roomToUserMappings' 
    }

createUUID :: IO UUID
createUUID = nextRandom

createRoom :: UUID -> Text -> ServerState -> ServerState
createRoom id name state =
    let 
        rooms' = M.insert id (Room id name) $ rooms state 
        usedRoomName' = S.insert name $ usedRoomNames state
    in state { rooms = rooms', usedRoomNames = usedRoomName' }

broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    T.putStrLn message
    forM_ (M.elems (users state)) $ \user -> WS.sendTextData (connection user) message


main :: IO ()
main = do
    putStrLn "Starting server"
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state


application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application mVarState pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        state <- readMVar mVarState
        lname <- return $ loginName <$> (decode msg :: Maybe Connect)
        userId <- createUUID
        case lname of
            Just name   | any ($ name)
                            [T.null, T.any isPunctuation, T.any isSpace] ->
                                WS.sendClose conn ("Name cannot " <>
                                    "contain punctuation or whitespace, and " <>
                                    "cannot be empty" :: Text)
                        | nameTaken name state ->
                            WS.sendClose conn ("User already exists" :: Text)
                        | otherwise -> 
                            
                            flip finally (disconnect userId) $ do
                            modifyMVar_ mVarState $ \s -> do
                                
                                let s' = addUser userId name conn s
                                -- WS.sendTextData conn $
                                --      "Welcome! Users: " <>
                                --      T.intercalate ", " (M.map fst s)
                                broadcast (name <> " joined") s'
                                return s'
                            talk name conn mVarState
            Nothing -> WS.sendClose conn ("Not working" :: Text) --test that this actually closes a connection
            where
            disconnect userId = do
                -- Remove user and return new state
                s <- modifyMVar mVarState $ \s ->
                    let s' = removeUser userId s in return (s', s')
                return s
                -- broadcast ((" disconnected" :: Text)) s

-- The talk function continues to read messages from a single user until he
-- disconnects. All messages are broadcasted to the other users.
talk :: Text -> WS.Connection -> MVar ServerState -> IO ()
talk user conn state = forever $ do
    msg <- WS.receiveData conn
    actionM <- return $ (decode msg :: Maybe Action)
    case actionM of
        Just (action) -> case action of
                                CreateRoom name -> modifyMVar_ state $ \s -> do
                                    uuid <- createUUID
                                    let s' = createRoom uuid name s
                                    WS.sendTextData conn $ encode $ RoomCreated uuid
                                    return s'
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