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

main :: IO ()
main = hspec $ do
  describe "Requests" $ do
    describe "Decode requests" $ do
      it "Can decode login" $ do
        decode ("{\"request\":\"LOGIN\",\"username\":\"Bob\"}") `shouldBe` (Just $ (Req.Login "Bob"))
      it "Can decode logout" $ do
        decode ("{\"request\":\"LOGOUT\"}") `shouldBe` (Just Req.Logout)
      it "Can decode send room msg" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"SEND_ROOM_MSG\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"data\":\"Test\"}") `shouldBe` (Just (Req.SendMsgRoom expectedUUID "Test"))
      it "Can decode create room" $ do
        decode ("{\"request\":\"CREATE_ROOM\",\"room_name\":\"Test room name\"}") `shouldBe` (Just $ Req.CreateRoom "Test room name")
      it "Can decode join room" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"JOIN_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.JoinRoom expectedUUID)
      it "Can decode leave room" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"LEAVE_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.LeaveRoom expectedUUID)
      it "Can decode destroy room" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"DESTROY_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.DestroyRoom expectedUUID)
      it "Can decode list all rooms" $ do
        decode ("{\"request\":\"LIST_ALL_ROOMS\"}") `shouldBe` (Just Req.ListAllRooms)
      it "Can decode list room members" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        decode ("{\"request\":\"LIST_ROOM_MEMBERS\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}") `shouldBe` (Just $ Req.ListRoomMembers expectedUUID)
    describe "Encode requests" $ do
      it "Can encode login" $ do
        encode (Req.Login "Bob") `shouldBe` "{\"request\":\"LOGIN\",\"username\":\"Bob\"}"
      it "Can encode logout" $ do
        encode Req.Logout `shouldBe` "{\"request\":\"LOGOUT\"}"
      it "Can encode send room msg" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.SendMsgRoom expectedUUID "Test") `shouldBe` "{\"request\":\"SEND_ROOM_MSG\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"data\":\"Test\"}"
      it "Can encode create room" $ do
        encode (Req.CreateRoom "Test room name") `shouldBe` "{\"request\":\"CREATE_ROOM\",\"room_name\":\"Test room name\"}"
      it "Can encode join room" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.JoinRoom expectedUUID) `shouldBe` "{\"request\":\"JOIN_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
      it "Can encode join room" $ do
        let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
        encode (Req.JoinRoom expectedUUID) `shouldBe` "{\"request\":\"JOIN_ROOM\",\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
  
  describe "Responses" $ do
    it "Can encode UserLoggedIn" $ do
      let uuid = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      encode (Resp.UserLoggedIn uuid ) `shouldBe` "{\"response\":\"USER_LOGGED_IN\",\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
    it "Can encode ListOfUsers" $ do
      let roomId = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3ca"
      let userId1 = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      let user1 = Resp.UserExport {userExportId = userId1, userExportName="Bob"}
      let userId2 = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cd"
      let user2 = Resp.UserExport {userExportId = userId2, userExportName="Alice"}
      encode (Resp.ListOfUsers roomId [user1, user2]) `shouldBe` "{\"response\":\"LIST_OF_USERS\"\
        \,\"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3ca\"\
        \,\"users\":[\
        \{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"username\":\"Bob\"}\
        \,{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cd\",\"username\":\"Alice\"}]}"