{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.DataSpec where


import Test.Hspec

import Data.Aeson (decode, encode)

import HasHub.Object.Milestone.Data


spec :: Spec
spec = do
  describe "parse github output json" $ do
    it "no due_on" $ do
      let exp = decode "{\"number\": 1, \"title\": \"sprint 1\", \"due_on\": null}" :: Maybe GitHubOutput
      let act = Just $ GitHubOutput (Number 1) (Title "sprint 1") Nothing
      exp `shouldBe` act

    it "with due_on" $ do
      let exp = decode "{\"number\": 1, \"title\": \"sprint 1\", \"due_on\": \"2018-04-30T23:59:59Z\"}" :: Maybe GitHubOutput
      let act = Just $ GitHubOutput (Number 1) (Title "sprint 1") (Just $ DueOn "2018-04-30T23:59:59Z")
      exp `shouldBe` act

  describe "parse zenhub output json" $ do
    it "no start_on" $ do
      let exp = decode "{\"start_date\": null}" :: Maybe StartOn
      let act = Nothing
      exp `shouldBe` act

    it "with start_on" $ do
      let exp = decode "{\"start_date\": \"2018-04-01T00:00:00Z\"}" :: Maybe StartOn
      let act = Just $ StartOn "2018-04-01T00:00:00Z"
      exp `shouldBe` act

  describe "parse github input json" $ do
    it "no due_on" $ do
      let exp = encode $ GitHubInput (Title "sprint 1") Nothing
      let act = "{\"title\":\"sprint 1\"}"
      exp `shouldBe` act

    it "with due_on" $ do
      let exp = encode $ GitHubInput (Title "sprint 1") (Just $ DueOn "2018-04-30T23:59:59Z")
      let act = "{\"due_on\":\"2018-04-30T23:59:59Z\",\"title\":\"sprint 1\"}"
      exp `shouldBe` act

  describe "show" $ do
    it "no start_on and no due_on" $ do
      let exp = show $ Milestone (Number 1) (Title "sprint 1") Nothing Nothing
      let act = "sprint 1 (           ~           )"
      exp `shouldBe` act

    it "no start_on and with due_on" $ do
      let exp = show $ Milestone (Number 1) (Title "sprint 1") Nothing (Just $ DueOn "2018-04-30T00:00:00Z")
      let act = "sprint 1 (           ~ 2018-04-30)"
      exp `shouldBe` act

    it "with start_on and no due_on" $ do
      let exp = show $ Milestone (Number 1) (Title "sprint 1") (Just $ StartOn "2018-04-01T00:00:00Z") Nothing
      let act = "sprint 1 (2018-04-01 ~           )"
      exp `shouldBe` act

    it "with start_on and with due_on" $ do
      let exp = show $ Milestone (Number 1) (Title "sprint 1") (Just $ StartOn "2018-04-01T00:00:00Z") (Just $ DueOn "2018-04-30T00:00:00Z")
      let act = "sprint 1 (2018-04-01 ~ 2018-04-30)"
      exp `shouldBe` act
