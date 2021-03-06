module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS
import qualified Requests as Requests
import qualified Responses as Responses
import qualified Broadcasts as Broadcasts
import Data.Aeson
import Data.Set as S
import Data.Maybe
import Data.Monoid
import qualified Network.WebSockets as WS
import Domain
import qualified Control.Logging as Log

data Room = Room {
    roomId :: RoomId,
    roomName :: Text
} deriving (Show)

data User = User {
    userId :: UserId,
    userName :: Text,
    connection :: WS.Connection
}

data ServerState = ServerState {
    users :: M.Map UserId User,
    rooms :: M.Map RoomId Room,
    userToRoomMappings :: M.Map UserId (S.Set RoomId),
    roomToUserMappings :: M.Map RoomId (S.Set UserId),
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

addUser :: UserId -> Text -> WS.Connection -> ServerState -> ServerState
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

logoutUser :: UserId -> ServerState -> ServerState
logoutUser id state =
    let
        userMap' = M.delete id (users state)
        userToRoomMappings' = M.delete id (userToRoomMappings state)
        roomToUserMappings' = M.map (\userSet -> S.delete id userSet) (roomToUserMappings state)
        usedUserNames' = fromMaybe (usedUserNames state) 
            $ (\user -> S.delete (userName user) $ usedUserNames state) <$> M.lookup id (users state)
    in state {
        users = userMap'
        , userToRoomMappings = userToRoomMappings'
        , roomToUserMappings = roomToUserMappings'
        , usedUserNames = usedUserNames'
    }

removeUser :: UserId -> ServerState -> ServerState
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

createRoom :: RoomId -> Text -> ServerState -> ServerState
createRoom id name state =
    let 
        rooms' = M.insert id (Room id name) $ rooms state 
        usedRoomName' = S.insert name $ usedRoomNames state
        roomToUserMappings' = M.insert id S.empty $ roomToUserMappings state
    in state { rooms = rooms', usedRoomNames = usedRoomName', roomToUserMappings = roomToUserMappings' }

destroyRoom :: RoomId -> ServerState -> ServerState
destroyRoom roomId state =
    let
        userToRoomMappings' = M.map (\roomSet -> S.delete roomId roomSet) $ userToRoomMappings state
        roomToUserMappings' = M.delete roomId $ roomToUserMappings state
        roomsMap' = M.delete roomId $ rooms state
        usedRoomNames' = fromMaybe (usedRoomNames state) 
            $ (\room -> S.delete (roomName room) $ usedRoomNames state) <$> M.lookup roomId (rooms state)
    in state { 
        userToRoomMappings = userToRoomMappings'
        , roomToUserMappings = roomToUserMappings'
        , rooms = roomsMap'
        , usedRoomNames = usedRoomNames' 
    }

joinRoom :: UserId -> RoomId -> ServerState -> ServerState
joinRoom userId roomId state = 
    let
        userToRoomMappings' = M.adjust (\roomSet -> S.insert roomId roomSet) userId $ userToRoomMappings state
        roomToUserMappings' = M.adjust (\userSet -> S.insert userId userSet) roomId $ roomToUserMappings state
    in state { userToRoomMappings = userToRoomMappings', roomToUserMappings = roomToUserMappings' }

leaveRoom :: UserId -> RoomId -> ServerState -> ServerState
leaveRoom userId roomId state = 
    let
        userToRoomMappings' = M.adjust (\roomSet -> S.delete roomId roomSet) userId  $ userToRoomMappings state
        roomToUserMappings' = M.adjust (\userSet -> S.delete userId userSet) roomId $ roomToUserMappings state
    in state { userToRoomMappings = userToRoomMappings', roomToUserMappings = roomToUserMappings' }

listAllRooms :: ServerState -> [Room]
listAllRooms = M.elems . rooms

listRoomMembers :: RoomId -> ServerState -> [User]
listRoomMembers id state = 
    let
        userIdList = S.toList $(M.findWithDefault S.empty id $ roomToUserMappings state :: S.Set UserId)
        userMap = users state :: M.Map UserId User
        userList = catMaybes $ Prelude.map (\userId' -> (M.lookup userId' userMap)) userIdList
    in userList

sendRoomMessage :: ServerState -> UserId -> RoomId -> Text -> IO ()
sendRoomMessage state fromUserId toRoomId msg' =
    let
        userIdList = S.toList $(M.findWithDefault S.empty toRoomId $ (roomToUserMappings state) :: S.Set UserId)
        userMap = users state :: M.Map UserId User
        userList = catMaybes $ Prelude.map (\userId' -> (M.lookup userId' userMap)) userIdList
        fromUserName = userName <$> M.lookup fromUserId userMap
    in do
        case fromUserName of
            Just uname -> 
                let 
                    jsonMessage = encodeJson $ Broadcasts.RoomMessage {roomId=toRoomId, userId=fromUserId, username=uname, msg = msg' }
                in do
                    Log.log jsonMessage
                    forM_ userList $ \user -> WS.sendTextData (connection user) jsonMessage
            Nothing -> Log.log "No username found" 

broadcast :: ServerState -> Text -> IO ()
broadcast state message = do
    Log.log message
    forM_ (M.elems (users state)) $ \user -> WS.sendTextData (connection user) message

respondToUser :: WS.Connection -> Responses.Response -> IO ()
respondToUser conn response = 
    do
        let msg = encodeJson response
        Log.log msg
        WS.sendTextData conn $ msg

closeConnection :: WS.Connection -> Responses.Response -> IO ()
closeConnection conn response =
    do 
        let msg = encodeJson response
        Log.log msg
        WS.sendClose conn $ msg

broadcastToRoom :: ServerState -> RoomId -> Broadcasts.Broadcast -> IO ()
broadcastToRoom state toRoomId broadcast  =
    let
        userIdList = S.toList $(M.findWithDefault S.empty toRoomId $ (roomToUserMappings state) :: S.Set UserId)
        userMap = users state :: M.Map UserId User
        userList = catMaybes $ Prelude.map (\userId' -> (M.lookup userId' userMap)) userIdList
        msg = encodeJson broadcast
    in do
        Log.log msg
        forM_ userList $ \user -> WS.sendTextData (connection user) msg

broadcastToRoomExceptUser :: ServerState -> RoomId -> UserId -> Broadcasts.Broadcast -> IO ()
broadcastToRoomExceptUser state toRoomId exceptUserId broadcast =
    let
        userIdList = S.toList $ S.filter (/= exceptUserId) $(M.findWithDefault S.empty toRoomId $ (roomToUserMappings state) :: S.Set UserId)
        userMap = users state :: M.Map UserId User
        userList = catMaybes $ Prelude.map (\userId' -> (M.lookup userId' userMap)) userIdList
        msg = encodeJson broadcast
    in do
        Log.log msg
        forM_ userList $ \user -> WS.sendTextData (connection user) msg

broadcastExceptToUser :: ServerState -> UserId -> Broadcasts.Broadcast -> IO ()
broadcastExceptToUser state fromUserId broadcast  =
    let
        msg = encodeJson broadcast
    in do
        Log.log msg
        forM_ (Prelude.filter (\u -> (userId u) /= fromUserId) $ M.elems (users state)) $ \user -> WS.sendTextData (connection user) msg

encodeJson :: ToJSON a => a -> Text
encodeJson = T.decodeUtf8 . BS.toStrict . encode

main :: IO ()
main = Log.withStdoutLogging $ do
    Log.log "Starting server"
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 8765 $ application state


-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application :: MVar ServerState -> WS.ServerApp
application mVarState pending = do
    conn <- WS.acceptRequest pending
    Log.log "Someone is reaching out"
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        Log.log $ T.decodeUtf8 $ BS.toStrict $ msg
        state <- readMVar mVarState
        let requsetM = (decode msg :: Maybe Requests.Request)
        userId <- createUserId
        case requsetM of
            Just (Requests.Login name)
                | any ($ name)
                    [T.null, T.any isPunctuation, T.any isSpace] ->
                        closeConnection conn $ Responses.Error "Name cannot contain punctuation or whitespace, and cannot be empty. Closing connection. Please reconnect and try again."
                | nameTaken name state ->
                    closeConnection conn $ Responses.Error "User already exists. Closing connection. Please reconnect and try again."
                | otherwise -> 
                    flip finally (disconnect userId) $ do
                    modifyMVar_ mVarState $ \s -> do
                        
                        let s' = addUser userId name conn s
                        respondToUser conn $ Responses.UserLoggedIn userId
                        return s'
                    talk userId conn mVarState
            Just _ -> closeConnection conn $ Responses.Error "Expected a login request. Closing connection; user must send login request immediately after opening a connection."
            Nothing -> do
                closeConnection conn $ Responses.Error "The request was not identified. Closing connection; user must send login request immediately after opening a connection."
            where
            disconnect userId = do
                -- Remove user and return new state
                s <- modifyMVar mVarState $ \s ->
                    let s' = removeUser userId s in return (s', s')
                return s

-- The talk function continues to read messages from a single user until he
-- disconnects. All messages are broadcasted to the other users.
talk :: UserId -> WS.Connection -> MVar ServerState -> IO ()
talk userId' conn mVarState = forever $ do
    msg <- WS.receiveData conn
    Log.log $ ("Message received: " :: Text) <> (T.decodeUtf8 . BS.toStrict $ msg)
    let actionM = (decode msg :: Maybe Requests.Request)
    case actionM of
        Just (action) -> 
            case action of
                Requests.Logout -> modifyMVar_ mVarState $ \oldState -> do
                    let newState = logoutUser userId' oldState
                    let roomsLoggingOutOf = S.toList $ M.findWithDefault S.empty userId' $ userToRoomMappings oldState
                    broadcastExceptToUser newState userId' $ Broadcasts.UserLoggedOut userId' roomsLoggingOutOf
                    respondToUser conn $ Responses.UserLoggedOut userId'
                    return newState
                Requests.CreateRoom name -> modifyMVar_ mVarState $ \s -> do
                    roomId' <- createRoomId
                    let s' = createRoom roomId' name s
                    respondToUser conn $ Responses.RoomCreated roomId' name
                    return s'
                Requests.JoinRoom roomId' -> modifyMVar_ mVarState $ \s -> do
                    let s' = joinRoom userId' roomId' s
                    let username' = fromJust $ userName <$> M.lookup userId' (users s)
                    broadcastToRoom s roomId' $ Broadcasts.UserJoinedRoom {roomId=roomId', userId=userId', username=username'}
                    let users = (\u -> Responses.UserExport { userExportId = (userId u), userExportName = (userName u) }) <$> listRoomMembers roomId' s'
                    respondToUser conn $ Responses.RoomJoined roomId' users
                    return s'
                Requests.LeaveRoom roomId' -> modifyMVar_ mVarState $ \s -> do
                    let s' = leaveRoom userId' roomId' s
                    broadcastToRoom s' roomId' $ Broadcasts.UserLeftRoom {roomId=roomId', userId=userId'}
                    respondToUser conn $ Responses.RoomLeft roomId'
                    return s'
                Requests.ListAllRooms -> do 
                    s <- readMVar mVarState
                    let rooms = (\r -> Responses.RoomExport { roomExportId = (roomId r), roomExportName = (roomName r) }) <$> listAllRooms s
                    respondToUser conn $ Responses.ListOfRooms rooms
                Requests.ListRoomMembers roomId' -> do 
                    s <- readMVar mVarState
                    let users = (\u -> Responses.UserExport { userExportId = (userId u), userExportName = (userName u) }) <$> listRoomMembers roomId' s
                    respondToUser conn $ Responses.ListOfUsers roomId' users
                Requests.SendMsgRoom roomId' msg -> do 
                    s <- readMVar mVarState
                    sendRoomMessage s userId' roomId' msg 
                Requests.DestroyRoom roomId' -> modifyMVar_ mVarState $ \s -> do
                    let s' = destroyRoom roomId' s
                    broadcastToRoomExceptUser s roomId' userId' $ Broadcasts.RoomDestroyed roomId'
                    respondToUser conn $ Responses.RoomDestroyed roomId'
                    return s'
                Requests.Login _ -> respondToUser conn $ Responses.Error "User is already logged in."
        Nothing -> respondToUser conn $ Responses.Error "The request was not identified."