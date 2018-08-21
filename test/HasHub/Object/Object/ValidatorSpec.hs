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

      it "format error is ignored" $ do
        let act = [QuestionEpicNumber "?", QuestionEpicNumber "1"] `areAllIn` []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "single in empty" $ do
        let act = [F.sharpEpicNumber] `areAllIn` []

        act `shouldBe` Failure [NonExistentError F.epicNumber1]

    describe "message" $ do
      it "error on #1" $ do
        let act = toMessage $ NonExistentError F.epicNumber1

        act `shouldBe` "no such epic: #1"

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
            DuplicationError $ EpicLinkNumber "?1"
          , DuplicationError $ EpicLinkNumber "?2"
          ]

    describe "message" $ do
      it "error on ?1" $ do
        let act = toMessage $ DuplicationError $ EpicLinkNumber "?1"

        act `shouldBe` "duplicate definition: ?1"

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
            FormatError $ EpicLinkNumber "1"
          , FormatError $ EpicLinkNumber "#1"
          , FormatError $ EpicLinkNumber "?"
          , FormatError $ EpicLinkNumber "? 1"
          , FormatError $ EpicLinkNumber "??1"
          ]

    describe "message" $ do
      it "error on question-epic-number" $ do
        let act = toMessage $ FormatError $ EpicLinkNumber "1"

        act `shouldBe` "not satisfied ^?\\d$ format: 1"

  describe "linking-number format" $ do
    describe "success" $ do
      it "starts with ? and number only, starts with # and number only" $ do
        let act = linkingNumberFormat [QuestionEpicNumber "?1", SharpEpicNumber "#1", SharpEpicNumber "#123"]

        act `shouldBe` Success ()

      it "empty" $ do
        let act = linkingNumberFormat []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "valid, sharp-only, sharp-and-contain-non-number, no-mark, double-mark" $ do
        let act = linkingNumberFormat [SharpEpicNumber "#1", SharpEpicNumber "#", SharpEpicNumber "# 1", QuestionEpicNumber "1", QuestionEpicNumber "?#"]

        act `shouldBe` Failure [
            FormatError $ SharpEpicNumber "#"
          , FormatError $ SharpEpicNumber "# 1"
          , FormatError $ QuestionEpicNumber "1"
          , FormatError $ QuestionEpicNumber "?#"
          ]

    describe "message" $ do
      it "error on sharp-epic-number" $ do
        let act = toMessage $ FormatError $ SharpEpicNumber "#"

        act `shouldBe` "not satisfied ^#\\d$ format: #"

    describe "message" $ do
      it "error on question-epic-number" $ do
        let act = toMessage $ FormatError $ QuestionEpicNumber "1"

        act `shouldBe` "not satisfied ^?\\d$ format: 1"

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

      it "define error is ignore format error" $ do
        let act = linking [(1, EpicLinkNumber "?")] [(1, QuestionEpicNumber "?")]

        act `shouldBe` Success ()

      it "not defined error is ignore format error" $ do
        let act = linking [] [(1, QuestionEpicNumber "?")]

        act `shouldBe` Success ()

    describe "failure" $ do
      it "no def, use at line-1" $ do
        let linking1 = (2, QuestionEpicNumber "?2")

        let act = linking [] [linking1]

        act `shouldBe` Failure [NotDefinedError linking1]

      it "def at line-1, use at line-2 but mismatch number" $ do
        let linked1  = (1, EpicLinkNumber     "?1")
        let linking1 = (2, QuestionEpicNumber "?2")

        let act = linking [linked1] [linking1]

        act `shouldBe` Failure [NotDefinedError linking1]

      it "def at line-1, use at line-1" $ do
        let linked1  = (1, EpicLinkNumber     "?1")
        let linking1 = (1, QuestionEpicNumber "?1")

        let act = linking [linked1] [linking1]

        act `shouldBe` Failure [DefineLineError linked1 linking1]

      it "def at line-2, use at line-1" $ do
        let linked1  = (2, EpicLinkNumber     "?1")
        let linking1 = (1, QuestionEpicNumber "?1")

        let act = linking [linked1] [linking1]

        act `shouldBe` Failure [DefineLineError linked1 linking1]

      it "def at line-1, use at line-1 and mismatch number and sharp on line-2 and mismatch number" $ do
        let linked1  = (1, EpicLinkNumber     "?1")
        let linking1 = (1, QuestionEpicNumber "?1")
        let linking2 = (2, SharpEpicNumber    "#1")
        let linking3 = (2, QuestionEpicNumber "?2")
        let linking4 = (3, QuestionEpicNumber "?2")

        let act = linking [linked1] [linking1, linking2, linking3, linking4]

        act `shouldBe` Failure [
            DefineLineError linked1 linking1
          , NotDefinedError linking3
          , NotDefinedError linking4
          ]

    describe "message" $ do
      it "define line error" $ do
        let linked     = (3, EpicLinkNumber     "?1")
        let linking    = (2, QuestionEpicNumber "?1")

        let act = toMessage $ DefineLineError linked linking

        act `shouldBe` "can't resolve linking epic: use ?1 on line 2, but ?1 is defined at line 3"

    describe "message" $ do
      it "not defined error" $ do
        let linking    = (2, QuestionEpicNumber "?1")

        let act = toMessage $ NotDefinedError (3, QuestionEpicNumber "?2")

        act `shouldBe` "can't resolve linking epic: use ?2 on line 3, but ?2 is not defined"
