{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Label.IOType

import HasHub.Connection.Config.Type (QueryParser(..), PaginationQueryParser(..))

import qualified Fixture as F


spec :: Spec
spec = do
  describe "refer labels" $ do
    it "first input" $ do
      let act = toQueryPart ReferLabelsInput F.owner F.repository Nothing

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    labels(first:100) {"
        , "      nodes {"
        , "        name"
        , "      }"
        , "      pageInfo {"
        , "        hasNextPage"
        , "        endCursor"
        , "      }"
        , "    }"
        , "  }"
        , "}"
        ]

    it "second input" $ do
      let act = toQueryPart ReferLabelsInput F.owner F.repository F.cursor

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    labels(first:100, after:\"abcd==\") {"
        , "      nodes {"
        , "        name"
        , "      }"
        , "      pageInfo {"
        , "        hasNextPage"
        , "        endCursor"
        , "      }"
        , "    }"
        , "  }"
        , "}"
        ]

    it "first output" $ do
      let sut = "{\"data\":{\"repository\":{\"labels\":{\"nodes\":[{\"name\": \"\229\174\159\232\163\133\"}, {\"name\": \"dev\"}],\"pageInfo\":{\"endCursor\":\"abcd==\",\"hasNextPage\":true}}}}}"

      asLabels sut `shouldReturn` [F.label1, F.label2]
      parseHasNext ReferLabelsInput sut `shouldReturn` True
      parseEndCursor ReferLabelsInput sut `shouldBe` (Just "abcd==")

    it "last output" $ do
      let sut = "{\"data\":{\"repository\":{\"labels\":{\"nodes\":[],\"pageInfo\":{\"endCursor\":null,\"hasNextPage\":false}}}}}"

      asLabels sut `shouldReturn` []
      parseHasNext ReferLabelsInput sut `shouldReturn` False
      parseEndCursor ReferLabelsInput sut `shouldBe` Nothing
