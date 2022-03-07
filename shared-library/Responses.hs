module Responses where

import Data.Text (Text)
import GHC.Generics
import Data.UUID
import Data.Aeson
import Control.Applicative (empty)

data RoomExport = RoomExport {
    roomExportId :: UUID,
    roomExportName :: Text
} deriving (Eq, Show)

instance FromJSON RoomExport where
    parseJSON (Object o) = RoomExport <$> o .: "roomId" <*> o .: "roomName"
    parseJSON _ = empty

instance ToJSON RoomExport where
    toJSON (RoomExport id name) = object ["roomId" .= id, "roomName" .= name]

data UserExport = UserExport {
    userExportId :: UUID,
    userExportName :: Text
} deriving (Eq, Show)

instance FromJSON UserExport where
    parseJSON (Object o) = UserExport <$> o .: "userId" <*> o .: "username"
    parseJSON _ = empty

instance ToJSON UserExport where
    toJSON (UserExport id name) = object ["userId" .= id, "username" .= name]

data RequestStatus = Success | Failure Text

data Response
    = UserLoggedIn UUID
    | UserLoggedOut UUID
    | RoomMsgSent
    | ListOfUsers [UserExport]
    | RoomCreated UUID Text
    | RoomJoined UUID
    | RoomLeft UUID
    | ListOfRooms [RoomExport]
    | RoomDestroyed UUID
    | Error Text
    deriving (Generic, Show)

instance FromJSON Response where
    parseJSON (Object o) =
        do 
        oc <- o .: "response"
        case oc of
            String "USER_LOGGED_IN" ->  UserLoggedIn <$> o .: "userId"
            String "USER_LOGGED_OUT" ->  UserLoggedOut <$> o .: "userId"
            String "SEND_ROOM_MSG" -> return RoomMsgSent
            String "LIST_OF_USERS" -> ListOfUsers <$> o .: "users" 
            String "ROOM_CREATED" ->  RoomCreated <$> o .: "roomId" <*> o .: "roomName"
            String "ROOM_JOINED" -> RoomJoined <$> o .: "roomId"
            String "ROOM_LEFT" -> RoomLeft <$> o .: "roomId"
            String "LIST_OF_ROOMS" -> ListOfRooms <$> o .: "rooms"
            String "ROOM_DESTROYED" -> RoomDestroyed <$> o .: "roomId"
            String "ERROR" -> Responses.Error <$> o .: "errorMessage"
            _ -> empty
    parseJSON _ = empty

instance ToJSON Response where
    toJSON (UserLoggedIn userId) = object ["response" .= ("USER_LOGGED_IN" :: Value), "userId" .= userId]
    toJSON (UserLoggedOut userId) = object ["response" .= ("USER_LOGGED_OUT" :: Value), "userId" .= userId]
    toJSON (RoomMsgSent) = object ["response" .= ("SEND_ROOM_MSG" :: Value)]
    toJSON (ListOfUsers users) = object ["response" .= ("LIST_OF_USERS" :: Value), "users" .= users]
    toJSON (RoomCreated roomId roomName) = object ["response" .= ("ROOM_CREATED" :: Value), "roomId" .= roomId, "roomName" .= roomName]
    toJSON (RoomJoined roomId) = object ["response" .= ("ROOM_JOINED" :: Value), "roomId" .= roomId]
    toJSON (RoomLeft roomId) = object ["response" .= ("ROOM_LEFT" :: Value), "roomId" .= roomId]
    toJSON (ListOfRooms rooms) = object ["response" .= ("LIST_OF_ROOMS" :: Value), "rooms" .= rooms]
    toJSON (RoomDestroyed roomId) = object ["response" .= ("ROOM_DESTROYED" :: Value), "roomId" .= roomId]
    toJSON (Responses.Error msg) = object ["response" .= ("ERROR" :: Value), "msg" .= msg]