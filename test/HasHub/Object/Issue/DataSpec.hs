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
      let exp = encode $ GitHubInput title body Nothing [] []
      let act = "{\"assignees\":[],\"body\":\"install java\",\"labels\":[],\"title\":\"machine setup\"}"
      exp `shouldBe` act

    it "title and body and milestone" $ do
      let exp = encode $ GitHubInput title body mm [] []
      let act = "{\"assignees\":[],\"body\":\"install java\",\"milestone\":1,\"labels\":[],\"title\":\"machine setup\"}"
      exp `shouldBe` act

    it "title and body and milestone and collaborators and labels" $ do
      let exp = encode $ GitHubInput title body mm cs ls
      let act = "{\"assignees\":[\"John\",\"Jane\"],\"body\":\"install java\",\"milestone\":1,\"labels\":[\"setup\"],\"title\":\"machine setup\"}"
      exp `shouldBe` act

  describe "parse epic input json" $ do
    it "issue and repository" $ do
      let exp = encode $ EpicInput (Number 2) 12345678
      let act = "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":12345678}]}"
      exp `shouldBe` act

  describe "parse pipeline input json" $ do
    it "pipeline" $ do
      let exp = encode $ PipelineInput (PipelineId "12345678")
      let act = "{\"pipeline_id\":\"12345678\",\"position\":\"bottom\"}"
      exp `shouldBe` act

  describe "parse estimate input json" $ do
    it "integer estimate" $ do
      let exp = encode $ EstimateInput (Estimate (1 :: Double))
      let act = "{\"estimate\":1}"
      exp `shouldBe` act

    it "double estimate" $ do
      let exp = encode $ EstimateInput (Estimate 0.5)
      let act = "{\"estimate\":0.5}"
      exp `shouldBe` act

  describe "show estimate" $ do
    it "integer estimate" $ do
      let exp = show $ Estimate (1 :: Double)
      let act = "1"
      exp `shouldBe` act

    it "double estimate" $ do
      let exp = show $ Estimate 0.5
      let act = "0.5"
      exp `shouldBe` act

    it "double estimate" $ do
      let exp = show $ Estimate 1.5
      let act = "1.5"
      exp `shouldBe` act
