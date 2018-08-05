{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.DataSpec where


import Test.Hspec

import Data.Aeson (decode)

import HasHub.Object.Pipeline.Data


spec :: Spec
spec = do
  describe "parse zenhub json" $ do
    it "id and name" $ do
      let exp = parseInList "{\"pipelines\":[{\"id\":\"1\",\"name\":\"backlog\"},{\"id\":\"2\",\"name\":\"sprint backlog\"}]}" :: Maybe [Pipeline]
      let act = Just [Pipeline "1" "backlog", Pipeline "2" "sprint backlog"]

      exp `shouldBe` act
