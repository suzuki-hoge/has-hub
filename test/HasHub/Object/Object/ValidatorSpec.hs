{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Object.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    describe "success" $ do
      it "all in" $ do
        let act = [F.sharpEpicNumber1] `areAllIn` [F.epicNumber1]

        act `shouldBe` Success ()

      it "question number is ignored" $ do
        let act = [F.questionEpicNumber1] `areAllIn` []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "single in empty" $ do
        let act = [F.sharpEpicNumber1] `areAllIn` []

        act `shouldBe` Failure [show F.epicNumber1]
