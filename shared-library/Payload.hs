{-# LANGUAGE DeriveGeneric #-}

module Payload where

import GHC.Generics
import Data.UUID
import Data.Aeson

data Action 
    = Connect String
    | CreateRoom String
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | SendMsgRoom UUID String
    deriving (Generic, Show)

instance FromJSON Action
instance ToJSON Action

data Response
    = UserDisconnected UUID
    | UserMessageToRoom UUID String
    | UserJoinedRoom UUID
    | UserCreated UUID
    | ListOfUsers [String]
    deriving (Generic, Show)

instance FromJSON Response
instance ToJSON Response

data IrcRequest = IrcRequest { action :: Action } deriving (Generic, Show)

instance FromJSON IrcRequest
instance ToJSON IrcRequest


data IrcResponse = IrcResponse {response :: Response} deriving (Generic, Show)

instance FromJSON IrcResponse
instance ToJSON IrcResponse