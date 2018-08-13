module HasHub.Object.Label.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Label.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    it "success" $ do
      let act = [F.label1] `areAllIn` [F.label1]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.label1] `areAllIn` []

      act `shouldBe` Failure [show F.label1]
