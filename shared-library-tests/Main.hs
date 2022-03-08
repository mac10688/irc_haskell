{-# LANGUAGE OverloadedStrings  #-}


module Main where

import Data.Function
import Data.Maybe
import GHC.Generics
import qualified Requests as Req
import qualified Responses as Resp
import Data.Aeson
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.QuickCheck
import Test.Hspec.Expectations
import Data.UUID
import Data.Maybe
import Domain

main :: IO ()
main = hspec $ do
  describe "Requests" $ do
    describe "Decode requests" $ do
      it "Can decode login request" $ do
        decode ("{\"request\":\"LOGIN\",\"username\":\"Bob\"}") `shouldBe` (Just $ (Req.Login "Bob"))
      it "Can decode logout request" $ do
        decode ("{\"request\":\"LOGOUT\"}") `shouldBe` (Just Req.Logout)
      it "Can decode send room msg request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"SEND_ROOM_MSG\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"data\":\"Test\"}") `shouldBe` (Just (Req.SendMsgRoom expectedRoomId "Test"))
      it "Can decode create room request" $ do
        decode ("{\"request\":\"CREATE_ROOM\",\"room_name\":\"Test room name\"}") `shouldBe` (Just $ Req.CreateRoom "Test room name")
      it "Can decode join room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"JOIN_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.JoinRoom expectedRoomId)
      it "Can decode leave room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"LEAVE_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.LeaveRoom expectedRoomId)
      it "Can decode destroy room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"DESTROY_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.DestroyRoom expectedRoomId)
      it "Can decode list all rooms request" $ do
        decode ("{\"request\":\"LIST_ALL_ROOMS\"}") `shouldBe` (Just Req.ListAllRooms)
      it "Can decode list room members request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"LIST_ROOM_MEMBERS\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.ListRoomMembers expectedRoomId)
    describe "Encode requests" $ do
      it "Can encode login request" $ do
        encode (Req.Login "Bob") `shouldBe` "{\"request\":\"LOGIN\",\"username\":\"Bob\"}"
      it "Can encode logout request" $ do
        encode Req.Logout `shouldBe` "{\"request\":\"LOGOUT\"}"
      it "Can encode send room msg request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.SendMsgRoom expectedRoomId "Test") `shouldBe` "{\"data\":\"Test\",\"request\":\"SEND_ROOM_MSG\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode create room request" $ do
        encode (Req.CreateRoom "Test room name") `shouldBe` "{\"request\":\"CREATE_ROOM\",\"room_name\":\"Test room name\"}"
      it "Can encode join room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.JoinRoom expectedRoomId) `shouldBe` "{\"request\":\"JOIN_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode leave room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.LeaveRoom expectedRoomId) `shouldBe` "{\"request\":\"LEAVE_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode destroy room request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.DestroyRoom expectedRoomId) `shouldBe` "{\"request\":\"DESTROY_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode list all rooms request" $ do
        encode (Req.ListAllRooms) `shouldBe` "{\"request\":\"LIST_ALL_ROOMS\"}"
      it "Can encode list room members request" $ do
        let expectedRoomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.ListRoomMembers expectedRoomId) `shouldBe` "{\"request\":\"LIST_ROOM_MEMBERS\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"

  describe "Responses" $ do
    describe "Can decode responses" $ do
      it "Can decode login response" $ do
        let expectedUserId = UserId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"response\":\"USER_LOGGED_IN\",\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ (Resp.UserLoggedIn expectedUserId))
      it "Can decode logout response" $ do
        let expectedUserId = UserId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"response\":\"USER_LOGGED_OUT\",\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ (Resp.UserLoggedOut expectedUserId))
      -- it "Can decode list of users response" $ do
      --   let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      --   decode ("{\"response\":\"LIST_OF_USERS\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ (Resp.ListOfUsers expectedUUID [{}]))
    describe "Can encode responses" $ do
      it "Can encode UserLoggedIn" $ do
        let expectedUserId = UserId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Resp.UserLoggedIn expectedUserId ) `shouldBe` "{\"response\":\"USER_LOGGED_IN\",\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode ListOfUsers" $ do
        let roomId = RoomId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3ca"
        let userId1 = UserId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        let user1 = Resp.UserExport {userExportId = userId1, userExportName="Bob"}
        let userId2 = UserId $ fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cd"
        let user2 = Resp.UserExport {userExportId = userId2, userExportName="Alice"}
        encode (Resp.ListOfUsers roomId [user1, user2]) `shouldBe` "{\"response\":\"LIST_OF_USERS\"\
          \,\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3ca\"\
          \,\"users\":[\
          \{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"username\":\"Bob\"}\
          \,{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cd\",\"username\":\"Alice\"}]}"