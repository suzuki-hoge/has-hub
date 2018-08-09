{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.DataSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Object.Data

import HasHub.Object.Milestone.Data
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data hiding (parseInList)

import qualified HasHub.Fixture as F


spec :: Spec
spec = do
  describe "parse create issue input json" $ do
    it "title and body" $ do
      let exp = encode $ CreateIssueInput F.title F.body F.noMilestone F.noCollaborators F.noLabels
      let act = "{\"assignees\":[],\"body\":\"post user data and write record.\",\"labels\":[],\"title\":\"post module\"}"
      act `shouldBe` exp

    it "title and body and milestone and collaborators and labels" $ do
      let exp = encode $ CreateIssueInput F.title F.body F.justMilestone F.collaborators F.labels
      let act = "{\"assignees\":[\"suzuki-hoge\"],\"body\":\"post user data and write record.\",\"milestone\":1,\"labels\":[\"setup\",\"dev\"],\"title\":\"post module\"}"
      act `shouldBe` exp

  describe "parse set epic input json" $ do
    it "issue and repository" $ do
      let exp = encode $ SetEpicInput (IssueNumber 2) 12345678
      let act = "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":12345678}]}"
      act `shouldBe` exp

  describe "parse set pipeline input json" $ do
    it "pipeline" $ do
      let exp = encode $ SetPipelineInput F.pipeline
      let act = "{\"pipeline_id\":\"12345678\",\"position\":\"bottom\"}"
      act `shouldBe` exp

  describe "parse set estimate input json" $ do
    it "integer estimate" $ do
      let exp = encode $ SetEstimateInput (Estimate (1 :: Double))
      let act = "{\"estimate\":1}"
      act `shouldBe` exp

    it "double set estimate" $ do
      let exp = encode $ SetEstimateInput (Estimate 0.5)
      let act = "{\"estimate\":0.5}"
      act `shouldBe` exp

  describe "show estimate" $ do
    it "integer estimate" $ do
      let exp = "1"
      let act = show $ Estimate (1 :: Double)
      act `shouldBe` exp

    it "double estimate" $ do
      let exp = "0.5"
      let act = show $ Estimate 0.5
      act `shouldBe` exp

    it "double estimate" $ do
      let exp = "1.5"
      let act = show $ Estimate 1.5
      act `shouldBe` exp

  describe "parse zenhub json" $ do
    it "number" $ do
      let exp = Just [EpicNumber 1, EpicNumber 2]
      let act = parseInList "{\"epic_issues\":[{\"issue_number\":1},{\"issue_number\":2}]}"
      act `shouldBe` exp

  describe "show epic" $ do
    it "number and title" $ do
      let exp = "EpicNumber 1 Title \"machine setup\""
      let act = show $ Epic (EpicNumber 1) (Title "machine setup")
      act `shouldBe` exp

  describe "intersect" $ do
    it "find [2] in case [?1 -> 2] and [?1]" $ do
      let exp = [EpicNumber 2]
      let act = fixEpicNumbers [LinkedEpic (EpicQuestionNumber 1) (EpicNumber 2)] [EpicQuestionNumber 1]
      act `shouldBe` exp

    it "use [1] in case [?1 -> 2] and [#1] " $ do
      let exp = [EpicNumber 1]
      let act = fixEpicNumbers [LinkedEpic (EpicQuestionNumber 1) (EpicNumber 2)] [EpicSharpNumber 1]
      act `shouldBe` exp

    it "not found in case [?1 -> 2] and []" $ do
      let exp = []
      let act = fixEpicNumbers [LinkedEpic (EpicQuestionNumber 1) (EpicNumber 2)] []
      act `shouldBe` exp

    it "fixed [1, 3] in case [?1 -> 2, ?2 -> 3] and [#1, ?2] " $ do
      let exp = [EpicNumber 1, EpicNumber 3]
      let act = fixEpicNumbers [LinkedEpic (EpicQuestionNumber 1) (EpicNumber 2), LinkedEpic (EpicQuestionNumber 2) (EpicNumber 3)] [EpicSharpNumber 1, EpicQuestionNumber 2]
      act `shouldBe` exp
