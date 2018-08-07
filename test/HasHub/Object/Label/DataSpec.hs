{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Label.Data


spec :: Spec
spec = do
  describe "parse github json" $ do
    it "name" $ do
      let exp = Just $ Label "bug"
      let act = decode "{\"name\": \"bug\"}"
      act `shouldBe` exp

    it "multi byte name" $ do
      let exp = Just $ Label "実装"
      let act = decode "{\"name\": \"\229\174\159\232\163\133\"}"
      act `shouldBe` exp

