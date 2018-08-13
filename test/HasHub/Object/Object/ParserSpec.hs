module HasHub.Object.Object.ParserSpec where


import Test.Hspec

import HasHub.Object.Object.Parser

import qualified Fixture as F


spec :: Spec
spec = do
  describe "success" $ do
    it "minimum parameter epic" $ do
      act <- readObjects "test/yaml/objects//minimum_parameter_epic.yaml"

      act `shouldBe` Success [
          EpicYamlObject F.epicLinkNumber F.title1 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "minimum parameter issue" $ do
      act <- readObjects "test/yaml/objects//minimum_parameter_epic.yaml"

      act `shouldBe` Success [
          EpicYamlObject F.epicLinkNumber F.title1 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "contained unknown key" $ do
      act <- readObjects "test/yaml/objects//contained_unknown_key_issue.yaml"

      act `shouldBe` Success [
          IssueYamlObject F.title2 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "epic and issue" $ do
      act <- readObjects "test/yaml/objects//epic_and_issue.yaml"

      act `shouldBe` Success [
          EpicYamlObject F.epicLinkNumber F.title1 F.body1 Nothing [] [] Nothing Nothing []
        , IssueYamlObject F.title2 F.body2 (Just F.pipelineName2) [F.label2] [F.collaborator] (Just F.milestoneTitle2) (Just F.estimate2) [F.questionEpicNumber, F.sharpEpicNumber]
        ]
