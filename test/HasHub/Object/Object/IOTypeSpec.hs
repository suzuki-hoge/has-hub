{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.IOTypeSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Object.IOType

import HasHub.Connection.Config.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer issues" $ do
      let act = toResource ReferIssueInput

      act `shouldBe` "/issues"

    it "refer epics" $ do
      let act = toResource ReferEpicInput

      act `shouldBe` "/epics"

    it "title, body, labels, collaborators, milestone" $ do
      let sut = CreateIssueInput F.title1 F.body1 [F.label1, F.label2] [F.collaborator] (Just F.milestone1)

      toResource sut `shouldBe` "/issues"
      encode sut `shouldBe` "{\"assignees\":[\"John\"],\"body\":\"post user data and write record.\",\"milestone\":1,\"labels\":[\"\229\174\159\232\163\133\",\"dev\"],\"title\":\"registration module\"}"

    it "title, body, no labels, no collaborators, no milestone" $ do
      let sut = CreateIssueInput F.title1 F.body1 [] [] Nothing

      toResource sut `shouldBe` "/issues"
      encode sut `shouldBe` "{\"assignees\":[],\"body\":\"post user data and write record.\",\"labels\":[],\"title\":\"registration module\"}"

    it "pipeline" $ do
      let sut = SetPipelineInput F.issueNumber F.pipeline1

      toResource sut `shouldBe` "/issues/2/moves"
      encode sut `shouldBe` "{\"pipeline_id\":\"5b02c59d2133e10681389873\",\"position\":\"bottom\"}"

    it "int estimate" $ do
      let sut = SetEstimateInput F.issueNumber F.estimate1

      toResource sut `shouldBe` "/issues/2/estimate"
      encode sut `shouldBe` "{\"estimate\":3}"

    it "double estimate" $ do
      let sut = SetEstimateInput F.issueNumber F.estimate2

      toResource sut `shouldBe` "/issues/2/estimate"
      encode sut `shouldBe` "{\"estimate\":0.5}"

    it "epic" $ do
      let sut = SetEpicInput F.issueNumber F.epicNumber1 F.repositoryId

      toResource sut `shouldBe` "/epics/1/update_issues"
      encode sut `shouldBe` "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":131509978}]}"

    it "to epic" $ do
      let sut = ConvertToEpicInput F.issueNumber

      toResource sut `shouldBe` "/issues/2/convert_to_epic"
      encode sut `shouldBe` "{}"

  describe "output" $ do
    it "issue-outputs" $ do
      let act = asIssueOutputs "[{\"number\": 2, \"title\": \"user registration api\"}]"

      act `shouldBe` [F.referIssueOutput]

    it "epic-numbers" $ do
      let act = asEpicNumbers "{\"epic_issues\": [{\"issue_number\": 1}, {\"issue_number\": 2}]}"

      act `shouldBe` [F.epicNumber1, F.epicNumber2]

    it "issue-number" $ do
      let act = asIssueNumber "{\"number\": 2}"

      act `shouldBe` F.issueNumber
