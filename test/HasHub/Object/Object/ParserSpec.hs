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

  describe "extract functions" $ do
    let epic1 = EpicYamlObject  (EpicLinkNumber "?1") (Title "foo") (Body "foo.") (Just $ PipelineName "backlog")        [Label "dev",  Label "java"]   [Collaborator "John"] Nothing (Just $ Estimate 1) [QuestionEpicNumber "?1"]
    let epic2 = EpicYamlObject  (EpicLinkNumber "?1") (Title "foo") (Body "foo.") (Just $ PipelineName "sprint backlog") [Label "java"]                 [Collaborator "Jane"] Nothing (Just $ Estimate 1) [SharpEpicNumber "#"]
    let epic3 = EpicYamlObject  (EpicLinkNumber "?3") (Title "foo") (Body "foo.") (Just $ PipelineName "sprint backlog") [Label "java", Label "oracle"] []                    Nothing (Just $ Estimate 1) [QuestionEpicNumber "?1", QuestionEpicNumber "?2"]
    let issue = IssueYamlObject                       (Title "foo") (Body "foo.") (Just $ PipelineName "doing")          [Label "dev",  Label "oracle"] []                    Nothing (Just $ Estimate 1) [QuestionEpicNumber "?"]

    let objects = [epic1, epic2, epic3, issue]

    it "epicLinkNumbers" $ do
      let act = _epicLinkNumbers objects

      act `shouldBe` [EpicLinkNumber "?1", EpicLinkNumber "?3"]

    it "epicLinkNumbersWithDuplication" $ do
      let act = _epicLinkNumbersWithDuplication objects

      act `shouldBe` [EpicLinkNumber "?1", EpicLinkNumber "?1", EpicLinkNumber "?3"]

    it "pipelineNames" $ do
      let act = _pipelineNames objects

      act `shouldBe` [PipelineName "backlog", PipelineName "sprint backlog", PipelineName "doing"]

    it "labels" $ do
      let act = _labels objects

      act `shouldBe` [Label "dev", Label "java", Label "oracle"]

    it "collaborators" $ do
      let act = _collaborators objects

      act `shouldBe` [Collaborator "John", Collaborator "Jane"]

    it "milestoneTitles" $ do
      let act = _milestoneTitles objects

      act `shouldBe` []

    it "parentEpicNumbers" $ do
      let act = _parentEpicNumbers objects

      act `shouldBe` [QuestionEpicNumber "?1", SharpEpicNumber "#", QuestionEpicNumber "?2", QuestionEpicNumber "?"]

    it "definitionEpicLinkNumbers" $ do
      let act = _definitionEpicLinkNumbers objects

      act `shouldBe` [(1, EpicLinkNumber "?1"), (2, EpicLinkNumber "?1"), (3, EpicLinkNumber "?3")]

    it "parentEpicLinkNumbers" $ do
      let act = _parentEpicLinkNumbers objects

      act `shouldBe` [(1, QuestionEpicNumber "?1"), (2, SharpEpicNumber "#"), (3, QuestionEpicNumber "?1"), (3, QuestionEpicNumber "?2"), (4, QuestionEpicNumber "?")]
