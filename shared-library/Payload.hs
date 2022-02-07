{-# LANGUAGE DeriveGeneric #-}

module Payload where

import GHC.Generics
import Data.UUID
import Data.Aeson

data Action 
    = CreateRoom String
    | JoinRoom UUID
    | LeaveRoom UUID
    | ListAllRooms
    | ListRoomMembers UUID
    | SendMsgRoom UUID String
    deriving (Generic, Show)

instance FromJSON Action
instance ToJSON Action

data IrcPayload = IrcPayload {
      id :: UUID
    , action :: Action
} deriving (Generic, Show)

instance FromJSON IrcPayload
instance ToJSON IrcPayload