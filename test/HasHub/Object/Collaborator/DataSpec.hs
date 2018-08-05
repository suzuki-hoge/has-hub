{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Collaborator.Data


spec :: Spec
spec = do
  describe "parse github json" $ do
    it "name" $ do
      let exp = decode "{\"login\": \"suzuki-hoge\"}" :: Maybe Collaborator
      let act = Just $ Collaborator "suzuki-hoge"
      exp `shouldBe` act
