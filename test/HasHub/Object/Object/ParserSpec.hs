module HasHub.Object.Object.ParserSpec where


import Test.Hspec

import Data.Either.Validation

import HasHub.Object.Object.Parser

import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "success" $ do
    it "minimum parameter epic" $ do
      act <- readObjects "test/dummies/yaml/objects/minimum_parameter_epic.yaml"

      act `shouldBe` Success [
          YamlEpic F.epicLinkNumber F.title1 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "minimum parameter issue" $ do
      act <- readObjects "test/dummies/yaml/objects/minimum_parameter_epic.yaml"

      act `shouldBe` Success [
          YamlEpic F.epicLinkNumber F.title1 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "contained unknown key" $ do
      act <- readObjects "test/dummies/yaml/objects/contained_unknown_key_issue.yaml"

      act `shouldBe` Success [
          YamlIssue F.title2 F.emptyBody Nothing [] [] Nothing Nothing []
        ]

    it "epic and issue" $ do
      act <- readObjects "test/dummies/yaml/objects/epic_and_issue.yaml"

      act `shouldBe` Success [
          YamlEpic F.epicLinkNumber F.title1 F.body1 Nothing [] [] Nothing Nothing []
        , YamlIssue F.title2 F.body2 (Just F.pipelineName2) [F.label2] [F.collaborator] (Just F.milestoneTitle2) (Just F.estimate2) [F.questionEpicNumber, F.sharpEpicNumber]
        ]

  describe "extract functions" $ do
    let epic1 = YamlEpic  (EpicLinkNumber "?1") (Title "foo") (Body "foo.") (Just $ PipelineName "backlog")        [Label "dev",  Label "java"]   [Collaborator "John"] Nothing (Just $ Estimate 1) [QuestionEpicNumber "?1"]
    let epic2 = YamlEpic  (EpicLinkNumber "?1") (Title "foo") (Body "foo.") (Just $ PipelineName "sprint backlog") [Label "java"]                 [Collaborator "Jane"] Nothing (Just $ Estimate 1) [SharpEpicNumber "#"]
    let epic3 = YamlEpic  (EpicLinkNumber "?3") (Title "foo") (Body "foo.") (Just $ PipelineName "sprint backlog") [Label "java", Label "oracle"] []                    Nothing (Just $ Estimate 1) [QuestionEpicNumber "?1", QuestionEpicNumber "?2"]
    let issue = YamlIssue                       (Title "foo") (Body "foo.") (Just $ PipelineName "doing")          [Label "dev",  Label "oracle"] []                    Nothing (Just $ Estimate 1) [QuestionEpicNumber "?"]

    let yamls = [epic1, epic2, epic3, issue]

    it "epicLinkNumbers" $ do
      let act = _epicLinkNumbers yamls

      act `shouldBe` [EpicLinkNumber "?1", EpicLinkNumber "?3"]

    it "epicLinkNumbersWithDuplication" $ do
      let act = _epicLinkNumbersWithDuplication yamls

      act `shouldBe` [EpicLinkNumber "?1", EpicLinkNumber "?1", EpicLinkNumber "?3"]

    it "pipelineNames" $ do
      let act = _pipelineNames yamls

      act `shouldBe` [PipelineName "backlog", PipelineName "sprint backlog", PipelineName "doing"]

    it "labels" $ do
      let act = _labels yamls

      act `shouldBe` [Label "dev", Label "java", Label "oracle"]

    it "collaborators" $ do
      let act = _collaborators yamls

      act `shouldBe` [Collaborator "John", Collaborator "Jane"]

    it "milestoneTitles" $ do
      let act = _milestoneTitles yamls

      act `shouldBe` []

    it "linkingEpicNumbers" $ do
      let act = _linkingEpicNumbers yamls

      act `shouldBe` [QuestionEpicNumber "?1", SharpEpicNumber "#", QuestionEpicNumber "?2", QuestionEpicNumber "?"]

    it "linkeds" $ do
      let act = _linkeds yamls

      act `shouldBe` [(1, EpicLinkNumber "?1"), (2, EpicLinkNumber "?1"), (3, EpicLinkNumber "?3")]

    it "linkings" $ do
      let act = _linkings yamls

      act `shouldBe` [(1, QuestionEpicNumber "?1"), (2, SharpEpicNumber "#"), (3, QuestionEpicNumber "?1"), (3, QuestionEpicNumber "?2"), (4, QuestionEpicNumber "?")]
