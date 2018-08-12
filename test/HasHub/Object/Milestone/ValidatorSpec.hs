{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Milestone.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    it "success" $ do
      let act = [F.milestoneTitle1] `areAllIn` [F.milestone1]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.milestoneTitle1] `areAllIn` []

      act `shouldBe` Failure [show F.milestoneTitle1]
