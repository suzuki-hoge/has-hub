module HasHub.Object.Object.ValidatorSpec where


import Test.Hspec

import HasHub.Object.Object.Validator
import HasHub.Object.Object.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "are all in" $ do
    describe "success" $ do
      it "all in" $ do
        let act = [F.sharpEpicNumber] `areAllIn` [F.epic]

        act `shouldBe` Success ()

      it "question number is ignored" $ do
        let act = [F.questionEpicNumber] `areAllIn` []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "single in empty" $ do
        let act = [F.sharpEpicNumber] `areAllIn` []

        act `shouldBe` Failure [show F.epicNumber1]

  describe "no duplication" $ do
    describe "success" $ do
      it "double" $ do
        let act = noDuplication [EpicLinkNumber "?1", EpicLinkNumber "?2"]

        act `shouldBe` Success ()

      it "single" $ do
        let act = noDuplication [EpicLinkNumber "?1"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = noDuplication []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "double" $ do
        let act = noDuplication [EpicLinkNumber "?2", EpicLinkNumber "?1", EpicLinkNumber "?3", EpicLinkNumber "?1", EpicLinkNumber "?2"]

        act `shouldBe` Failure [
            "duplicate error: ?1"
          , "duplicate error: ?2"
          ]

  describe "link-number format" $ do
    describe "success" $ do
      it "starts with ? and number only" $ do
        let act = linkNumberFormat [EpicLinkNumber "?1", EpicLinkNumber "?123"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = linkNumberFormat []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "valid, no-mark, sharp-mark, no-number, contain-non-number, double-mark" $ do
        let act = linkNumberFormat [EpicLinkNumber "?1", EpicLinkNumber "1", EpicLinkNumber "#1", EpicLinkNumber "?", EpicLinkNumber "? 1", EpicLinkNumber "??1"]

        act `shouldBe` Failure [
            "format error: 1"
          , "format error: #1"
          , "format error: ?"
          , "format error: ? 1"
          , "format error: ??1"
          ]

  describe "parent-number format" $ do
    describe "success" $ do
      it "starts with ? and number only, starts with # and number only" $ do
        let act = parentNumberFormat [QuestionEpicNumber "?1", SharpEpicNumber "#1", SharpEpicNumber "#123"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = parentNumberFormat []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "valid, sharp-only, sharp-and-contain-non-number, no-mark, double-mark" $ do
        let act = parentNumberFormat [SharpEpicNumber "#1", SharpEpicNumber "#", SharpEpicNumber "# 1", QuestionEpicNumber "1", QuestionEpicNumber "?#"]

        act `shouldBe` Failure [
            "format error: #"
          , "format error: # 1"
          , "format error: 1"
          , "format error: ?#"
          ]

  describe "linking" $ do
    describe "success" $ do
      it "def at line-1, use at line-2" $ do
        let act = linking [(1, EpicLinkNumber "?1")] [(2, QuestionEpicNumber "?1")]

        act `shouldBe` Success ()

      it "def at line-1, no use" $ do
        let act = linking [(1, EpicLinkNumber "?1")] []

        act `shouldBe` Success ()

      it "def at line-1, sharp on line-1" $ do
        let act = linking [(1, EpicLinkNumber "?1")] [(1, SharpEpicNumber "#1")]

        act `shouldBe` Success ()

      it "def at line-1 and line-2, use at line-2 and line-4 and sharp on line-3" $ do
        let act = linking [(1, EpicLinkNumber "?1"), (2, EpicLinkNumber "?2")] [(2, QuestionEpicNumber "?1"), (3, SharpEpicNumber "#1"), (4, QuestionEpicNumber "?2")]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = linking [] []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "no def, use at line-1" $ do
        let act = linking [] [(1, QuestionEpicNumber "?1")]

        act `shouldBe` Failure ["use ?1 on line 1, but ?1 is not defined"]

      it "def at line-1, use at line-2 but mismatch number" $ do
        let act = linking [(1, EpicLinkNumber "?1")] [(2, QuestionEpicNumber "?2")]

        act `shouldBe` Failure ["use ?2 on line 2, but ?2 is not defined"]

      it "def at line-1, use at line-1" $ do
        let act = linking [(1, EpicLinkNumber "?1")] [(1, QuestionEpicNumber "?1")]

        act `shouldBe` Failure ["use ?1 on line 1, but ?1 is defined at line 1"]

      it "def at line-2, use at line-1" $ do
        let act = linking [(2, EpicLinkNumber "?1")] [(1, QuestionEpicNumber "?1")]

        act `shouldBe` Failure ["use ?1 on line 1, but ?1 is defined at line 2"]

      it "def at line-1, use at line-1 and mismatch number and sharp on line-2 and mismatch number" $ do
        let act = linking [(1, EpicLinkNumber "?1")] [(1, QuestionEpicNumber "?1"), (2, SharpEpicNumber "#1"), (2, QuestionEpicNumber "?2"), (3, QuestionEpicNumber "?2")]

        act `shouldBe` Failure [
            "use ?1 on line 1, but ?1 is defined at line 1"
          , "use ?2 on line 2, but ?2 is not defined"
          , "use ?2 on line 3, but ?2 is not defined"
          ]
