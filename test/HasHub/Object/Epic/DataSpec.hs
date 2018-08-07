{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Epic.DataSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Epic.Data


spec :: Spec
spec = do
  describe "parse zenhub json" $ do
    it "number" $ do
      let exp = Just [Number 1, Number 2]
      let act = parseInList "{\"epic_issues\":[{\"issue_number\":1},{\"issue_number\":2}]}"
      act `shouldBe` exp

  describe "show epic" $ do
    it "number and title" $ do
      let exp = "#1 machine setup"
      let act = show $ Epic (Number 1) (Title "machine setup")
      act `shouldBe` exp
