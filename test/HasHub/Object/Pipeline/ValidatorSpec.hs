module HasHub.Object.Pipeline.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Pipeline.Validator

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    it "success" $ do
      let act = [F.pipelineName1] `areAllIn` [F.pipeline1]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.pipelineName1] `areAllIn` []

      act `shouldBe` Failure [NonExistentError F.pipelineName1]

    it "message" $ do
      let act = toMessage $ NonExistentError F.pipelineName1

      act `shouldBe` "no such pipeline: backlog"
