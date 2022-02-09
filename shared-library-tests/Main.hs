{-# LANGUAGE OverloadedStrings  #-}


module Main where

import Data.Function
import Data.Maybe
import GHC.Generics
import Payload
import Data.Aeson
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.QuickCheck
import Test.Hspec.Expectations
import Data.UUID
import Data.Maybe

main :: IO ()
main = hspec $ do
  describe "Payload" $ do
    it "IrcPayload to json" $ do
      let uuid = fromMaybe (undefined::UUID) $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
      encode (IrcPayload uuid "Bob" ListAllRooms) `shouldBe` "{\"action\":{\"tag\":\"ListAllRooms\"},\"id\":\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\"}"