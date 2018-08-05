{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Epic.DataSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Epic.Data


spec :: Spec
spec = do
  describe "parse zenhub json" $ do
    it "number" $ do
      let exp = parseInList "{\"epic_issues\":[{\"issue_number\":1},{\"issue_number\":2}]}" :: Maybe [Number]
      let act = Just [Number 1, Number 2]

      exp `shouldBe` act

  describe "show epic" $ do
    it "number and title" $ do
      let exp = show $ Epic (Number 1) (Title "machine setup")
      let act = "#1 machine setup"
      exp `shouldBe` act
