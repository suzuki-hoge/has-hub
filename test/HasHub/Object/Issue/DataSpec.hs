{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Issue.DataSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Issue.Data

import qualified HasHub.Object.Milestone.Data as M
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data


spec :: Spec
spec = do
  describe "parse github input json" $ do
    let title = Title "machine setup"
    let body = Body "install java"
    let mm = Just $ M.Number 1
    let cs = [Collaborator "John", Collaborator "Jane"]
    let ls = [Label "setup"]

    it "title and body" $ do
      let exp = "{\"assignees\":[],\"body\":\"install java\",\"labels\":[],\"title\":\"machine setup\"}"
      let act = encode $ GitHubInput title body Nothing [] []
      act `shouldBe` exp

    it "title and body and milestone" $ do
      let exp = "{\"assignees\":[],\"body\":\"install java\",\"milestone\":1,\"labels\":[],\"title\":\"machine setup\"}"
      let act = encode $ GitHubInput title body mm [] []
      act `shouldBe` exp

    it "title and body and milestone and collaborators and labels" $ do
      let exp = "{\"assignees\":[\"John\",\"Jane\"],\"body\":\"install java\",\"milestone\":1,\"labels\":[\"setup\"],\"title\":\"machine setup\"}"
      let act = encode $ GitHubInput title body mm cs ls
      act `shouldBe` exp

  describe "parse epic input json" $ do
    it "issue and repository" $ do
      let exp = "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":12345678}]}"
      let act = encode $ EpicInput (Number 2) 12345678
      act `shouldBe` exp

  describe "parse pipeline input json" $ do
    it "pipeline" $ do
      let exp = "{\"pipeline_id\":\"12345678\",\"position\":\"bottom\"}"
      let act = encode $ PipelineInput (PipelineId "12345678")
      act `shouldBe` exp

  describe "parse estimate input json" $ do
    it "integer estimate" $ do
      let exp = "{\"estimate\":1}"
      let act = encode $ EstimateInput (Estimate (1 :: Double))
      act `shouldBe` exp

    it "double estimate" $ do
      let exp = "{\"estimate\":0.5}"
      let act = encode $ EstimateInput (Estimate 0.5)
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
