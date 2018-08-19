{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.TypeSpec where


import Test.Hspec

import HasHub.Object.Milestone.Type


spec :: Spec
spec = do
  describe "to string" $ do
    describe "start on" $ do
      it "nothing" $ do
        let act = _startOnString Nothing

        act `shouldBe` "          "

      it "just" $ do
        let act = _startOnString (Just $ StartOn "2018-01-01T00:00:00.000Z")

        act `shouldBe` "2018-01-01"

    describe "due on" $ do
      it "nothing" $ do
        let act = _dueOnString Nothing

        act `shouldBe` "          "

      it "just" $ do
        let act = _dueOnString (Just $ DueOn "2018-01-31T23:59:59Z")

        act `shouldBe` "2018-01-31"
