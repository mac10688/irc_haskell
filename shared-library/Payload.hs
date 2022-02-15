{-# LANGUAGE DeriveGeneric #-}

module Payload where

import Data.Text (Text)
import GHC.Generics
import Data.UUID
import Data.Aeson

data Connect = Connect {
    loginName :: Text
} deriving (Generic, Show)

instance FromJSON Connect
instance ToJSON Connect

data Action 
    = CreateRoom Text
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | SendMsgRoom UUID Text
    deriving (Generic, Show)

instance FromJSON Action
instance ToJSON Action

data Response
    = UserLoggedIn UUID
    | UserMessageToRoom UUID Text
    | UserJoinedRoom UUID
    | UserCreated UUID
    | ListOfUsers [Text]
    | RoomCreated 
    deriving (Generic, Show)

instance FromJSON Response
instance ToJSON Response

data IrcRequest = IrcRequest { action :: Action } deriving (Generic, Show)

instance FromJSON IrcRequest
instance ToJSON IrcRequest


data IrcResponse = IrcResponse {response :: Response} deriving (Generic, Show)

instance FromJSON IrcResponse
instance ToJSON IrcResponse