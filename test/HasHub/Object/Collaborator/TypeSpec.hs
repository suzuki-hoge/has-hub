{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.TypeSpec where


import Test.Hspec

import HasHub.Object.Collaborator.Type

import qualified Fixture as F


spec :: Spec
spec = do
  describe "decode" $ do
    it "name" $ do
      let act = decodeJust "[{\"login\": \"suzuki-hoge\"}]"

      act `shouldBe` [F.collaborator1]
