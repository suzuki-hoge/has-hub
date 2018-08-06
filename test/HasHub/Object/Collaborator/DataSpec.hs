{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Collaborator.Data


spec :: Spec
spec = do
  describe "parse github json" $ do
    it "name" $ do
      let exp = Just $ Collaborator "suzuki-hoge"
      let act = decode "{\"login\": \"suzuki-hoge\"}"
      act `shouldBe` exp
