module Responses where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Control.Applicative (empty)
import Domain

data RoomExport = RoomExport {
    roomExportId :: RoomId,
    roomExportName :: Text
} deriving (Eq, Show)

instance FromJSON RoomExport where
    parseJSON (Object o) = RoomExport <$> o .: "roomId" <*> o .: "roomName"
    parseJSON _ = empty

instance ToJSON RoomExport where
    toJSON (RoomExport id name) = object ["roomId" .= id, "roomName" .= name]

data UserExport = UserExport {
    userExportId :: UserId,
    userExportName :: Text
} deriving (Eq, Show)

instance FromJSON UserExport where
    parseJSON (Object o) = UserExport <$> o .: "userId" <*> o .: "username"
    parseJSON _ = empty

instance ToJSON UserExport where
    toJSON (UserExport id name) = object ["userId" .= id, "username" .= name]

data RequestStatus = Success | Failure Text

data Response
    = UserLoggedIn UserId
    | UserLoggedOut UserId
    | ListOfUsers RoomId [UserExport]
    | RoomCreated RoomId Text
    | RoomJoined RoomId [UserExport]
    | RoomLeft RoomId
    | ListOfRooms [RoomExport]
    | RoomDestroyed RoomId
    | Error Text
    deriving (Eq, Show)

instance FromJSON Response where
    parseJSON (Object o) =
        do 
        oc <- o .: "response"
        case oc of
            String "USER_LOGGED_IN" ->  UserLoggedIn <$> o .: "userId"
            String "USER_LOGGED_OUT" ->  UserLoggedOut <$> o .: "userId"
            String "LIST_OF_USERS" -> ListOfUsers <$> o .: "roomId" <*> o .: "users" 
            String "ROOM_CREATED" ->  RoomCreated <$> o .: "roomId" <*> o .: "roomName"
            String "ROOM_JOINED" -> RoomJoined <$> o .: "roomId" <*> o .: "users"
            String "ROOM_LEFT" -> RoomLeft <$> o .: "roomId"
            String "LIST_OF_ROOMS" -> ListOfRooms <$> o .: "rooms"
            String "ROOM_DESTROYED" -> RoomDestroyed <$> o .: "roomId"
            String "ERROR" -> Responses.Error <$> o .: "errorMessage"
            _ -> empty
    parseJSON _ = empty

instance ToJSON Response where
    toJSON (UserLoggedIn userId) = object ["response" .= ("USER_LOGGED_IN" :: Value), "userId" .= userId]
    toJSON (UserLoggedOut userId) = object ["response" .= ("USER_LOGGED_OUT" :: Value), "userId" .= userId]
    toJSON (ListOfUsers roomId' users) = object ["response" .= ("LIST_OF_USERS" :: Value), "roomId" .= roomId', "users" .= users]
    toJSON (RoomCreated roomId roomName) = object ["response" .= ("ROOM_CREATED" :: Value), "roomId" .= roomId, "roomName" .= roomName]
    toJSON (RoomJoined roomId users) = object ["response" .= ("ROOM_JOINED" :: Value), "roomId" .= roomId, "users" .= users]
    toJSON (RoomLeft roomId) = object ["response" .= ("ROOM_LEFT" :: Value), "roomId" .= roomId]
    toJSON (ListOfRooms rooms) = object ["response" .= ("LIST_OF_ROOMS" :: Value), "rooms" .= rooms]
    toJSON (RoomDestroyed roomId) = object ["response" .= ("ROOM_DESTROYED" :: Value), "roomId" .= roomId]
    toJSON (Responses.Error msg) = object ["response" .= ("ERROR" :: Value), "msg" .= msg]