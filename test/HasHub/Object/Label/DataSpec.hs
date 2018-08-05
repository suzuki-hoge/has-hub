{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Label.Data


spec :: Spec
spec = do
  describe "parse github json" $ do
    it "name" $ do
      let exp = decode "{\"name\": \"bug\"}" :: Maybe Label
      let act = Just $ Label "bug"
      exp `shouldBe` act

    it "multi byte name" $ do
      let exp = decode "{\"name\": \"\229\174\159\232\163\133\"}" :: Maybe Label
      let act = Just $ Label "実装"
      exp `shouldBe` act
