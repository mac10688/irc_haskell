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
    it "Can decode login" $ do
      decode ("{\"request\":\"LOGIN\", \"username\":\"Bob\"}") `shouldBe` (Just $ (Req.Login "Bob"))
    it "Can decode logout" $ do
      decode ("{\"request\":\"LOGOUT\"}") `shouldBe` (Just Req.Logout)
    it "Can decode send room msg" $ do
      let expectedUUID = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      decode ("{\"request\":\"SEND_ROOM_MSG\", \"roomId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\", \"data\":\"Test\"}") `shouldBe` (Just (Req.SendMsgRoom expectedUUID "Test"))
  
  describe "Responses" $ do
    it "Can encode UserLoggedIn" $ do
      let uuid = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      encode (Resp.UserLoggedIn uuid ) `shouldBe` "{\"response\":\"USER_LOGGED_IN\",\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\"}"
    it "Can encode ListOfUsers" $ do
      let userId1 = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cf"
      let user1 = Resp.UserExport {userExportId = userId1, userExportName="Bob"}
      let userId2 = fromJust $ fromString "eb3e9255-991c-4b0b-98ba-712cb1a0d3cd"
      let user2 = Resp.UserExport {userExportId = userId2, userExportName="Alice"}
      encode (Resp.ListOfUsers [user1, user2]) `shouldBe` "{\"response\":\"LIST_OF_USERS\",\"users\":[\
        \{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cf\",\"username\":\"Bob\"}\
        \,{\"userId\":\"eb3e9255-991c-4b0b-98ba-712cb1a0d3cd\",\"username\":\"Alice\"}]}"