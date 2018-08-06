{-# LANGUAGE OverloadedStrings #-}


module HasHub.Yaml.ParserSpec where


import Test.Hspec

import Data.Either.Validation (Validation(..))

import HasHub.Yaml.Parser


spec :: Spec
spec = do
  let emptyBody = ""
  let noEpics = []
  let noEstimate = Nothing
  let noMilestone = Nothing
  let noLabels = []
  let noAssignees = []
  let noPipeline = Nothing

  describe "parse objects yaml file" $ do
    describe "parse success" $ do
      it "minimum parameter epic" $ do
        let exp = Success $ [EpicObject "?2" "post module" emptyBody noEpics noEstimate noMilestone noLabels noAssignees noPipeline]
        let act = parseObjects "test/yaml/success/minimum_parameter_epic.yaml"
        act `shouldReturn` exp

      it "full parameter epic" $ do
        let exp = Success $ [EpicObject "?2" "post module" "post user data and write recode." ["#1"] (Just 3.0) (Just "sprint 1") ["dev"] ["John"] (Just "sprint backlog")]
        let act = parseObjects "test/yaml/success/full_parameter_epic.yaml"
        act `shouldReturn` exp

      it "epic and issue" $ do
        let exp = Success $ [EpicObject "?2" "post module" "post user data and write recode." ["#1"] noEstimate noMilestone noLabels noAssignees noPipeline, IssueObject "user api" "write recode." ["?2"] (Just 0.5) (Just "sprint 1") ["dev"] ["John"] (Just "sprint backlog")]
        let act = parseObjects "test/yaml/success/epic_and_issue.yaml"
        act `shouldReturn` exp

      it "contained unknown key" $ do
        let exp = Success $ [EpicObject "?2" "post module" emptyBody noEpics noEstimate noMilestone noLabels noAssignees noPipeline]
        let act = parseObjects "test/yaml/success/contained_unknown_key.yaml"
        act `shouldReturn` exp

    describe "parse failure" $ do
      it "invalid yaml" $ do
        let exp = Failure ["invalid yaml file"]
        let act = parseObjects "test/yaml/failure/invalid_yaml.yaml"
        act `shouldReturn` exp

      it "no yaml file" $ do
        let exp = Failure ["no such File(test/yaml/failure/no_yaml_file.yaml)"]
        let act = parseObjects "test/yaml/failure/no_yaml_file.yaml"
        act `shouldReturn` exp
