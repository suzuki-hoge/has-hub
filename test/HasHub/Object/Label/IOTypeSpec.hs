{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Label.IOType

import HasHub.Connection.Config.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer labels" $ do
      let act = toResource ReferInput

      act `shouldBe` "/labels"

  describe "output" $ do
    it "name" $ do
      let act = asLabels "[{\"name\": \"\229\174\159\232\163\133\"}, {\"name\": \"dev\"}]"

      act `shouldBe` [F.label1, F.label2]
