module Requests where

import Data.Text (Text)
import GHC.Generics
import Data.UUID
import Data.Aeson
import Control.Applicative (empty)

data Request 
    = Login Text
    | Logout
    | CreateRoom Text
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | DestroyRoom UUID
    | SendMsgRoom UUID Text
    deriving (Eq, Show)

instance FromJSON Request where
    parseJSON (Object o) =
        do 
            oc <- o .: "request"
            case oc of
                String "LOGIN" ->  Login <$> o .: "username"
                String "LOGOUT" -> return Logout
                String "SEND_ROOM_MSG" ->  SendMsgRoom <$> o .: "roomId" <*> o .: "data"
                String "CREATE_ROOM" -> CreateRoom <$> o .: "room_name"
                String "JOIN_ROOM" -> JoinRoom <$> o .: "roomId"
                String "LEAVE_ROOM" -> LeaveRoom <$> o .: "roomId"
                String "DESTROY_ROOM" -> DestroyRoom <$> o .: "roomId"
                String "LIST_ALL_ROOMS" -> return ListAllRooms
                String "LIST_ROOM_MEMBERS" -> ListRoomMembers <$> o .: "roomId"
                _ -> empty
    parseJSON _ = empty

instance ToJSON Request where
    toJSON (Login name) = object ["request" .= String "LOGIN", "username" .= name]
    toJSON (Logout) = object ["request" .= ("LOGOUT" :: Value)]
    toJSON (SendMsgRoom roomId msg) = object ["request" .= ("SEND_ROOM_MESSAGE" :: Value), "roomId" .= roomId, "data" .= msg]
    toJSON (CreateRoom name) = object ["request" .= ("CREATE_ROOM" :: Value), "room_name" .= name]
    toJSON (JoinRoom roomId) = object ["request" .= ("JOIN_ROOM" :: Value), "roomId" .= roomId]
    toJSON (LeaveRoom roomId) = object ["request" .= ("LEAVE_ROOM" :: Value), "roomId" .= roomId]
    toJSON (DestroyRoom roomId) = object ["request" .= ("DESTROY_ROOM" :: Value), "roomId" .= roomId]
    toJSON (ListAllRooms) = object ["request" .= ("LIST_ALL_ROOMS" :: Value)]
    toJSON (ListRoomMembers roomId) = object ["request" .= ("LIST_ROOM_MEMBERS" :: Value), "roomId" .= roomId]
