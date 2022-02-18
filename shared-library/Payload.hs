{-# LANGUAGE DeriveGeneric #-}

module Payload where

import Data.Text (Text)
import GHC.Generics
import Data.UUID
import Data.Aeson

data Connect = Connect {
    loginName :: Text
} deriving (Generic, Show)

instance ToJSON Connect
instance FromJSON Connect

data Action 
    = CreateRoom Text
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | SendMsgRoom UUID Text
    deriving (Generic, Show)
instance ToJSON Action
instance FromJSON Action

data RoomExport = RoomExport {
    roomExportId :: UUID,
    roomExportName :: Text
} deriving (Generic, Show)

instance ToJSON RoomExport

data UserExport = UserExport {
    userExportId :: UUID,
    userExportName :: Text
} deriving (Generic, Show)

instance ToJSON UserExport

data ServerMessage
    = UserLoggedIn UUID
    | UserCreated UUID
    | ListOfUsers [UserExport]
    | RoomCreated UUID
    | ListOfRooms [RoomExport]
    | RoomMessage {toRoom :: UUID, fromUser :: Text, msg :: Text}
    | UserJoinedRoom {toRoom :: UUID, userWhoJoined :: UUID}
    | UserLeftRoom {toRoom :: UUID, userWhoJoined :: UUID}
    | Error Text
    deriving (Generic, Show)

instance ToJSON ServerMessage