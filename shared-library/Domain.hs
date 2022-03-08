module Domain where

import Data.UUID
import Data.UUID.V4
import Data.Aeson
import GHC.Generics

newtype UserId = UserId UUID deriving (Eq, Show, Ord, Generic)

createUserId :: IO UserId
createUserId = UserId <$> nextRandom

instance FromJSON UserId
instance ToJSON UserId

newtype RoomId = RoomId UUID deriving (Eq, Show, Ord, Generic)

createRoomId :: IO RoomId
createRoomId = RoomId <$> nextRandom

instance FromJSON RoomId
instance ToJSON RoomId