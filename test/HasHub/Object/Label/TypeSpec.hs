{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.TypeSpec where


import Test.Hspec

import HasHub.Object.Label.Type

import HasHub.Connection.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer" $ do
      let act = toResource ReferInput

      act `shouldBe` "/labels"

  describe "output" $ do
    it "name" $ do
      let act = decodeJust "[{\"name\": \"\229\174\159\232\163\133\"}, {\"name\": \"dev\"}]"

      act `shouldBe` [F.label1, F.label2]
