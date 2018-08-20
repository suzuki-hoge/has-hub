module HasHub.Object.Milestone.ParserSpec where


import Test.Hspec

import Data.Either.Validation

import HasHub.Object.Milestone.Parser

import HasHub.Object.Milestone.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "success" $ do
    it "minimum parameter epic" $ do
      act <- readObjects "test/dummies/yaml/milestones/minimum_parameter_milestone.yaml"

      act `shouldBe` Success [
          YamlMilestone F.milestoneTitle1 Nothing Nothing
        ]

    it "full parameter epic" $ do
      act <- readObjects "test/dummies/yaml/milestones/full_parameter_milestone.yaml"

      act `shouldBe` Success [
          YamlMilestone F.milestoneTitle1 F.startOn1 F.dueOn1
        ]

  describe "extract functions" $ do
    let milestone1 = YamlMilestone (MilestoneTitle "sprint 1") Nothing Nothing
    let milestone2 = YamlMilestone (MilestoneTitle "sprint 2") (Just $ StartOn "2018-04-01T00:00:00Z") (Just $ DueOn "2018-04-30T23:59:59Z")

    let yamls = [milestone1, milestone2]

    it "titles" $ do
      let act = _titles yamls

      act `shouldBe` [MilestoneTitle "sprint 1", MilestoneTitle "sprint 2"]

    it "start ons" $ do
      let act = _startOns yamls

      act `shouldBe` [StartOn "2018-04-01T00:00:00Z"]

    it "due ons" $ do
      let act = _dueOns yamls

      act `shouldBe` [DueOn "2018-04-30T23:59:59Z"]
