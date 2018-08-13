{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.TypeSpec where


import Test.Hspec

import HasHub.Object.Collaborator.Type

import HasHub.Connection.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer" $ do
      let act = toResource ReferInput

      act `shouldBe` "/collaborators"

  describe "output" $ do
    it "name" $ do
      let act = decodeJust "[{\"login\": \"suzuki-hoge\"}]"

      act `shouldBe` [F.collaborator]
