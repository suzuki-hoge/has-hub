{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Pipeline.IOType

import HasHub.Connection.Config.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "refer pipelines" $ do
    it "resource" $ do
      let act = toResource ReferPipelinesInput

      act `shouldBe` "/board"

    it "output" $ do
      let act = asPipelines "{\"pipelines\": [{\"id\": \"abc123\", \"name\":\"backlog\"}, {\"id\": \"xyz789\", \"name\": \"sprint backlog\"}]}"

      act `shouldReturn` [F.pipeline1, F.pipeline2]
