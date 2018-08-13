{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Pipeline.IOType

import HasHub.Connection.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer pipelines" $ do
      let act = toResource ReferInput

      act `shouldBe` "/board"

  describe "output" $ do
    it "id, name" $ do
      let act = asPipelines "{\"pipelines\": [{\"id\": \"5b02c59d2133e10681389873\", \"name\":\"backlog\"}, {\"id\": \"5b0577fa2133e1068138aabc\", \"name\": \"sprint backlog\"}]}"

      act `shouldBe` [F.pipeline1, F.pipeline2]
