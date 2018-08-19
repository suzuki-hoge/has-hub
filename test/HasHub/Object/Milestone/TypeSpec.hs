{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.TypeSpec where


import Test.Hspec

import HasHub.Object.Milestone.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "to string" $ do
    describe "start on" $ do
      it "just to just" $ do
        let act = _string F.milestoneTitle1 F.startOn1 F.dueOn1

        act `shouldBe` "sprint 1 (2018-04-01 ~ 2018-04-30)"

      it "just to nothing" $ do
        let act = _string F.milestoneTitle1 F.startOn1 F.dueOn2

        act `shouldBe` "sprint 1 (2018-04-01 ~           )"

      it "nothing to just" $ do
        let act = _string F.milestoneTitle1 F.startOn2 F.dueOn1

        act `shouldBe` "sprint 1 (           ~ 2018-04-30)"

      it "nothing to nothing" $ do
        let act = _string F.milestoneTitle1 F.startOn2 F.dueOn2

        act `shouldBe` "sprint 1 (           ~           )"
