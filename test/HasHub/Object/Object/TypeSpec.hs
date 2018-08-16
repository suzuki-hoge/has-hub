{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.TypeSpec where


import Test.Hspec

import HasHub.Object.Object.Type


spec :: Spec
spec = do
  describe "find in" $ do
    let s1 = SharpEpicNumber "#1"
    let s2 = SharpEpicNumber "#2"
    let s3 = SharpEpicNumber "#3"
    let q1 = QuestionEpicNumber "?1"
    let q2 = QuestionEpicNumber "?2"
    let e1 = EpicNumber 1
    let e2 = EpicNumber 2
    let e3 = EpicNumber 3
    let l1 = EpicLinkNumber "?1"
    let l2 = EpicLinkNumber "?2"

    describe "found" $ do
      it "return own at sharp number" $ do
        let act = findIn [] s1

        act `shouldBe` [e1]

      it "return own at sharp number" $ do
        let act = findIn [LinkedEpic l1 e2] s1

        act `shouldBe` [e1]

      it "return linking number at question number" $ do
        let act = findIn [LinkedEpic l1 e2] q1

        act `shouldBe` [e2]

      it "flat mapping" $ do
        let act = [q1, s3] >>= findIn [LinkedEpic l1 e1, LinkedEpic l2 e2]

        act `shouldBe` [e1, e3]

      it "empty be empty" $ do
        let act = [] >>= findIn [LinkedEpic l1 e1, LinkedEpic l2 e2]

        act `shouldBe` []
