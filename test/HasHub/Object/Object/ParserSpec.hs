{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.ParserSpec where


import Test.Hspec

import HasHub.Object.Object.Parser

import qualified Fixture as F


spec :: Spec
spec = do
  describe "success" $ do
    it "minimum parameter epic" $ do
      act <- readObjects "test/yaml/objects/success/minimum_parameter_epic.yaml"

      act `shouldBe` Success [EpicYamlObject F.epicLinkNumber1 F.title1 F.emptyBody Nothing [] [] Nothing Nothing []]

    it "full parameter epic" $ do
      act <- readObjects "test/yaml/objects/success/full_parameter_epic.yaml"

      act `shouldBe` Success [EpicYamlObject F.epicLinkNumber1 F.title1 F.body1 (Just F.pipelineName1) [F.label1] [F.collaborator1] (Just F.milestoneTitle1) (Just F.estimate1) [F.questionEpicNumber1, F.sharpEpicNumber1]]

    it "contained unknown key" $ do
      act <- readObjects "test/yaml/objects/success/contained_unknown_key.yaml"

      act `shouldBe` Success [EpicYamlObject F.epicLinkNumber1 F.title1 F.emptyBody Nothing [] [] Nothing Nothing []]

    it "epic and issue" $ do
      act <- readObjects "test/yaml/objects/success/epic_and_issue.yaml"

      act `shouldBe` Success [EpicYamlObject F.epicLinkNumber1 F.title1 F.body1 (Just F.pipelineName1) [] [] Nothing Nothing [], IssueYamlObject F.title2 F.body2 (Just F.pipelineName2) [F.label1] [F.collaborator1] (Just F.milestoneTitle2) (Just F.estimate2) [F.questionEpicNumber1]]
