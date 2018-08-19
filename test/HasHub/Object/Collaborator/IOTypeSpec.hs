{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Collaborator.IOType

import HasHub.Connection.Config.Type (toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "input" $ do
    it "refer collaborators" $ do
      let act = toResource ReferInput

      act `shouldBe` "/collaborators"

  describe "output" $ do
    it "name" $ do
      let act = asCollaborators "[{\"login\": \"John\"}]"

      act `shouldReturn` [F.collaborator]
