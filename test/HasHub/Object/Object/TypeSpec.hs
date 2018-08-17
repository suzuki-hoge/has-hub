{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.TypeSpec where


import Test.Hspec

import HasHub.Object.Object.Type


spec :: Spec
spec = do
  describe "find in" $ do
    let l1 = EpicLinkNumber "?1"
    let l2 = EpicLinkNumber "?2"

    let q1 = QuestionEpicNumber "?1"
    let q2 = QuestionEpicNumber "?2"

    let s3 = SharpEpicNumber "#3"
    let s4 = SharpEpicNumber "#4"

    let e1 = Epic (EpicNumber 1) (Title "title 1")
    let e2 = Epic (EpicNumber 2) (Title "title 2")
    let e3 = Epic (EpicNumber 3) (Title "title 3")
    let e4 = Epic (EpicNumber 4) (Title "title 4")

    let le1 = LinkedEpic l1 e1
    let le2 = LinkedEpic l2 e2

    it "found in linked-epics" $ do
      let act = [q1, q2] >>= findIn [le1, le2] [e3, e4]

      act `shouldBe` [e1, e2]

    it "found in referred epics" $ do
      let act = [s3, s4] >>= findIn [le1, le2] [e3, e4]

      act `shouldBe` [e3, e4]

    it "found in both" $ do
      let act = [q1, q2, s3, s4] >>= findIn [le1, le2] [e3, e4]

      act `shouldBe` [e1, e2, e3, e4]
