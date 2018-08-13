{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.TypeSpec where


import Test.Hspec

import HasHub.Object.Pipeline.Type

import HasHub.Connection.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer" $ do
      let act = toResource ReferInput

      act `shouldBe` "/board"

  describe "output" $ do
    it "id, name" $ do
      let act = decodeJust "{\"pipelines\": [{\"id\": \"5b02c59d2133e10681389873\", \"name\":\"backlog\"}, {\"id\": \"5b0577fa2133e1068138aabc\", \"name\": \"sprint backlog\"}]}"

      act `shouldBe` [F.pipeline1, F.pipeline2]
