module HasHub.Object.Collaborator.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Collaborator.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    it "success" $ do
      let act = [F.collaborator] `areAllIn` [F.collaborator]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.collaborator] `areAllIn` []

      act `shouldBe` Failure [NonExistentError F.collaborator]

    it "message" $ do
      let act = toMessage $ NonExistentError F.collaborator

      act `shouldBe` "no such assignee: suzuki-hoge"
