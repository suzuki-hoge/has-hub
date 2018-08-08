{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.DataSpec where


import Test.Hspec

import Data.Aeson (decode, encode)

import HasHub.Object.Milestone.Data


spec :: Spec
spec = do
  describe "parse github output json" $ do
    it "no due_on" $ do
      let exp = Just $ GitHubOutput (MilestoneNumber 1) (MilestoneTitle "sprint 1") Nothing
      let act = decode "{\"number\": 1, \"title\": \"sprint 1\", \"due_on\": null}"
      act `shouldBe` exp

    it "with due_on" $ do
      let exp = Just $ GitHubOutput (MilestoneNumber 1) (MilestoneTitle "sprint 1") (Just $ DueOn "2018-04-30T23:59:59Z")
      let act = decode "{\"number\": 1, \"title\": \"sprint 1\", \"due_on\": \"2018-04-30T23:59:59Z\"}"
      act `shouldBe` exp

  describe "parse zenhub output json" $ do
    it "no start_on" $ do
      let exp = Nothing
      let act = decode "{\"start_date\": null}" :: Maybe StartOn
      act `shouldBe` exp

    it "with start_on" $ do
      let exp = Just $ StartOn "2018-04-01T00:00:00Z"
      let act = decode "{\"start_date\": \"2018-04-01T00:00:00Z\"}"
      act `shouldBe` exp

  describe "parse github input json" $ do
    it "no due_on" $ do
      let exp = "{\"title\":\"sprint 1\"}"
      let act = encode $ GitHubInput (MilestoneTitle "sprint 1") Nothing
      act `shouldBe` exp

    it "with due_on" $ do
      let exp = "{\"due_on\":\"2018-04-30T23:59:59Z\",\"title\":\"sprint 1\"}"
      let act = encode $ GitHubInput (MilestoneTitle "sprint 1") (Just $ DueOn "2018-04-30T23:59:59Z")
      act `shouldBe` exp

  describe "show" $ do
    it "no start_on and no due_on" $ do
      let exp = "sprint 1 (           ~           )"
      let act = show $ Milestone (MilestoneNumber 1) (MilestoneTitle "sprint 1") Nothing Nothing
      act `shouldBe` exp

    it "no start_on and with due_on" $ do
      let exp = "sprint 1 (           ~ 2018-04-30)"
      let act = show $ Milestone (MilestoneNumber 1) (MilestoneTitle "sprint 1") Nothing (Just $ DueOn "2018-04-30T00:00:00Z")
      act `shouldBe` exp

    it "with start_on and no due_on" $ do
      let exp = "sprint 1 (2018-04-01 ~           )"
      let act = show $ Milestone (MilestoneNumber 1) (MilestoneTitle "sprint 1") (Just $ StartOn "2018-04-01T00:00:00Z") Nothing
      act `shouldBe` exp

    it "with start_on and with due_on" $ do
      let exp = "sprint 1 (2018-04-01 ~ 2018-04-30)"
      let act = show $ Milestone (MilestoneNumber 1) (MilestoneTitle "sprint 1") (Just $ StartOn "2018-04-01T00:00:00Z") (Just $ DueOn "2018-04-30T00:00:00Z")
      act `shouldBe` exp
