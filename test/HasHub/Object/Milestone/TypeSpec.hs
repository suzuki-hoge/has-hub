{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.TypeSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Milestone.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "decode" $ do
    it "number, title, due-on" $ do
      let act = decodeJust "[{\"number\": 1, \"title\": \"sprint 1\", \"due_on\": \"2018-04-30T23:59:59Z\"}, {\"number\": 2, \"title\": \"sprint 2\", \"due_on\": null}]"

      act `shouldBe` [F.referMilestoneOutput1, F.referMilestoneOutput2]

    it "start-on" $ do
      let act = decodeJust'' "{\"start_date\": \"2018-04-01T00:00:00Z\"}"

      act `shouldBe` F.startOn1

    it "no start-on" $ do
      let act = decodeJust'' "{\"start_date\": null}"

      act `shouldBe` F.startOn2
--
--  describe "encode" $ do
--    it "number, title, due-on" $ do
--      let act = encode F.createMilestoneInput1
--
--      act `shouldBe` "{\"due_on\":\"2018-04-30T23:59:59Z\",\"title\":\"sprint 1\"}"
--
--    it "number, title, no due-on" $ do
--      let act = encode F.createMilestoneInput2
--
--      act `shouldBe` "{\"title\":\"sprint 2\"}"
