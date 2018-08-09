{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Pipeline.Data

import qualified HasHub.Fixture as F


spec :: Spec
spec = do
  describe "parse zenhub json" $ do
    it "id and name" $ do
      let act = parseInList "{\"pipelines\":[{\"id\":\"1\",\"name\":\"backlog\"},{\"id\":\"2\",\"name\":\"sprint backlog\"}]}"
      let exp = Just [Pipeline "1" "backlog", Pipeline "2" "sprint backlog"]

      act `shouldBe` exp

  describe "intersect" $ do
    it "non-empty and just" $ do
      let exp = F.justPipeline
      let act = intersect [F.pipeline] F.justPipelineName
      act `shouldBe` exp

    it "non-empty and nothing" $ do
      let exp = Nothing
      let act = intersect [F.pipeline] Nothing
      act `shouldBe` exp

    it "empty and nothing" $ do
      let exp = Nothing
      let act = intersect [] Nothing
      act `shouldBe` exp
