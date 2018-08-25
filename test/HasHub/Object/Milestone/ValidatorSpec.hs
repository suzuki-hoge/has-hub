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

      act `shouldBe` Failure [NonExistentError F.milestoneTitle1]

    it "message" $ do
      let act = toMessage $ NonExistentError F.milestoneTitle1

      act `shouldBe` "no such milestone: sprint 1"

  describe "due on format" $ do
    describe "success" $ do
      it "valid" $ do
        let act = dueOnFormat [DueOn "2018-01-31T23:59:59Z"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = dueOnFormat []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "invalid" $ do
        let sut1 = DueOn "2018/01/31"
        let sut2 = DueOn "20180131"
        let act = dueOnFormat [sut1, sut2]

        act `shouldBe` Failure [FormatError sut1, FormatError sut2]

    describe "message" $ do
      it "message" $ do
        let act = toMessage $ FormatError $ DueOn "2018/01/31T23:59:59Z"

        act `shouldBe` "not match format(yyyy-mm-dd): 2018/01/31"

  describe "start on format" $ do
    describe "success" $ do
      it "valid" $ do
        let act = startOnFormat [StartOn "2018-01-01T00:00:00Z"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = startOnFormat []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "invalid" $ do
        let sut1 = StartOn "2018/01/01"
        let sut2 = StartOn "20180101"
        let act = startOnFormat [sut1, sut2]

        act `shouldBe` Failure [FormatError sut1, FormatError sut2]

    describe "message" $ do
      it "message" $ do
        let act = toMessage $ FormatError $ StartOn "2018/01/01T00:00:00Z"

        act `shouldBe` "not match format(yyyy-mm-dd): 2018/01/01"

  describe "are all not in" $ do
    it "success" $ do
      let act = [F.milestoneTitle1] `areAllNotIn` [F.milestone2]

      act `shouldBe` Success ()

    it "failure" $ do
      let act = [F.milestoneTitle1] `areAllNotIn` [F.milestone1]

      act `shouldBe` Failure [ExistingError F.milestoneTitle1]

    it "message" $ do
      let act = toMessage $ ExistingError F.milestoneTitle1

      act `shouldBe` "already existing: sprint 1"
