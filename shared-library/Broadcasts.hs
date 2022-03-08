module Broadcasts where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Control.Applicative (empty)
import Domain

data Broadcast =
      RoomMessage {roomId :: RoomId, userId :: UserId, username :: Text, msg :: Text}
    | UserJoinedRoom {roomId :: RoomId, userId :: UserId, username :: Text}
    | UserLeftRoom {roomId :: RoomId, userId :: UserId}
    | RoomDestroyed RoomId
    | UserLoggedOut {id :: UserId, roomsLoggingOut :: [RoomId]}


instance FromJSON Broadcast where
    parseJSON (Object o) =
        do 
        oc <- o .: "broadcast"
        case oc of
            String "ROOM_MESSAGE" ->  RoomMessage <$> o .: "roomId" <*> o .: "userId" <*> o .: "username" <*> o .: "msg"
            String "USER_JOINED_ROOM" ->  UserJoinedRoom <$> o .: "roomId" <*> o .: "userId" <*> o .: "username"
            String "USER_LEFT_ROOM" -> UserLeftRoom <$> o .: "roomId" <*> o .: "userId"
            String "ROOM_DESTROYED" -> RoomDestroyed <$> o .: "roomId"
            String "USER_LOGGED_OUT" ->  UserLoggedOut <$> o .: "userId" <*> o .: "rooms"
            _ -> empty
    parseJSON _ = empty

instance ToJSON Broadcast where
    toJSON (RoomMessage roomId' userId' username' msg') = object ["broadcast" .= ("ROOM_MESSAGE" :: Value), "roomId" .= roomId', "userId" .= userId', "username" .= username', "msg" .= msg']
    toJSON (UserJoinedRoom roomId' userId' username) = object ["broadcast" .= ("USER_JOINED_ROOM" :: Value), "roomId" .= roomId', "userId" .= userId', "username" .= username]
    toJSON (UserLeftRoom roomId' userId') = object ["broadcast" .= ("USER_LEFT_ROOM" :: Value), "roomId" .= roomId', "userId" .= userId']
    toJSON (RoomDestroyed roomId') = object ["broadcast" .= ("ROOM_DESTROYED" :: Value), "roomId" .= roomId']
    toJSON (UserLoggedOut userId' rooms') = object ["broadcast" .= ("USER_LOGGED_OUT" :: Value), "userId" .= userId', "rooms" .= rooms']