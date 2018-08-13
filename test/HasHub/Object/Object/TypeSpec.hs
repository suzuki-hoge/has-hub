{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.TypeSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Object.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "decode" $ do
    it "issue-number" $ do
      let act = decodeJust' "{\"number\": 2}"

      act `shouldBe` F.issueNumber

    it "epic-number" $ do
      let act = decodeJust "{\"epic_issues\": [{\"issue_number\": 1}, {\"issue_number\": 2}]}"

      act `shouldBe` [F.epicNumber1, F.epicNumber2]

  describe "encode" $ do
    it "title, body, labels, collaborators, milestone" $ do
      let act = encode $ CreateIssueInput F.title1 F.body1 [F.label1, F.label2] [F.collaborator] (Just F.milestone1)

      act `shouldBe` "{\"assignees\":[\"suzuki-hoge\"],\"body\":\"post user data and write record.\",\"milestone\":1,\"labels\":[\"\229\174\159\232\163\133\",\"dev\"],\"title\":\"registration module\"}"

    it "title, body, no labels, no collaborators, no milestone" $ do
      let act = encode $ CreateIssueInput F.title1 F.body1 [] [] Nothing

      act `shouldBe` "{\"assignees\":[],\"body\":\"post user data and write record.\",\"labels\":[],\"title\":\"registration module\"}"

    it "pipeline" $ do
      let act = encode $ SetPipelineInput F.pipeline1

      act `shouldBe` "{\"pipeline_id\":\"5b02c59d2133e10681389873\",\"position\":\"bottom\"}"

    it "int estimate" $ do
      let act = encode $ SetEstimateInput F.estimate1

      act `shouldBe` "{\"estimate\":3}"

    it "double estimate" $ do
      let act = encode $ SetEstimateInput F.estimate2

      act `shouldBe` "{\"estimate\":0.5}"

    it "epic" $ do
      let act = encode $ SetEpicInput F.issueNumber F.repositoryId

      act `shouldBe` "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":131509978}]}"
