{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.IOTypeSpec where


import Test.Hspec

import HasHub.Object.Collaborator.IOType

import HasHub.Connection.Config.Type (QueryParser(..), PaginationQueryParser(..))

import qualified Fixture as F


spec :: Spec
spec = do
  describe "refer collaborators" $ do
    it "first input" $ do
      let act = toQueryPart ReferCollaboratorsInput F.owner F.repository Nothing

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    assignableUsers(first:100) {"
        , "      nodes {"
        , "        login"
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
      let act = toQueryPart ReferCollaboratorsInput F.owner F.repository F.cursor

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    assignableUsers(first:100, after:\"abcd==\") {"
        , "      nodes {"
        , "        login"
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
      let sut = "{\"data\":{\"repository\":{\"assignableUsers\":{\"nodes\":[{\"login\": \"suzuki-hoge\"}],\"pageInfo\":{\"endCursor\":\"abcd==\",\"hasNextPage\":true}}}}}"

      asCollaborators sut `shouldReturn` [F.collaborator]
      parseHasNext ReferCollaboratorsInput sut `shouldReturn` True
      parseEndCursor ReferCollaboratorsInput sut `shouldBe` (Just "abcd==")

    it "last output" $ do
      let sut = "{\"data\":{\"repository\":{\"assignableUsers\":{\"nodes\":[],\"pageInfo\":{\"endCursor\":null,\"hasNextPage\":false}}}}}"

      asCollaborators sut `shouldReturn` []
      parseHasNext ReferCollaboratorsInput sut `shouldReturn` False
      parseEndCursor ReferCollaboratorsInput sut `shouldBe` Nothing
