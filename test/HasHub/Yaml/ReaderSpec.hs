{-# LANGUAGE OverloadedStrings #-}


module HasHub.Yaml.ReaderSpec where


import Test.Hspec

import Data.Aeson (FromJSON(..), Value(Object), (.:))

import HasHub.Yaml.Reader


newtype Foo = Foo String deriving (Eq, Show)
instance FromJSON Foo where
  parseJSON (Object v) = Foo <$> (v .: "foo")


sut :: FilePath -> IO (Validation [Error] [Foo])
sut = readYaml id


spec :: Spec
spec = do
  describe "failure" $ do
    it "invalid yaml file" $ do
      act <- sut "test/yaml/objects//epic_and_issue.yaml"

      act `shouldBe` Failure ["invalid yaml file"]

    it "no such file" $ do
      act <- sut "xxx/xxx.yaml"

      act `shouldBe` Failure ["no such File(xxx/xxx.yaml)"]
