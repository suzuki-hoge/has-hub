{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.TypeSpec where


import Test.Hspec

import HasHub.Object.Label.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "decode" $ do
    it "name" $ do
      let act = decodeJust "[{\"name\": \"setup\"}, {\"name\": \"\229\174\159\232\163\133\"}]"

      act `shouldBe` [F.label1, F.label2]
