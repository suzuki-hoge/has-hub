{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Collaborator.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    it "success" $ do
      let act = [F.collaborator1] `areAllIn` [F.collaborator1]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.collaborator1] `areAllIn` []

      act `shouldBe` Failure [show F.collaborator1]
