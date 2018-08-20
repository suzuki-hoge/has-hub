{-# LANGUAGE OverloadedStrings #-}


module HasHub.Yaml.ReaderSpec where


import Test.Hspec

import Data.Aeson (FromJSON(..), Value(Object), (.:))

import HasHub.Yaml.Reader


newtype Foo = Foo String deriving (Eq, Show)
instance FromJSON Foo where
  parseJSON (Object v) = Foo <$> (v .: "foo")


sut :: FilePath -> IO (Validation [YamlReadingError] [Foo])
sut = readYaml id


spec :: Spec
spec = do
  describe "failure" $ do
    it "invalid yaml file" $ do
      act <- sut "test/dummies/yaml/objects/epic_and_issue.yaml"

      act `shouldBe` Failure [YamlParseError]

    it "no such file" $ do
      act <- sut "xxx/xxx.yaml"

      act `shouldBe` Failure [NoYamlError "xxx/xxx.yaml"]

    describe "message" $ do
      it "yaml parse error" $ do
        let act = toMessage YamlParseError

        act `shouldBe` "yaml parse error"

      it "no yaml error" $ do
        let act = toMessage $ NoYamlError "xxx/xxx.yaml"

        act `shouldBe` "no yaml error: xxx/xxx.yaml"
