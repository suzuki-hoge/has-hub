{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.IOTypeSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Object.IOType

import HasHub.Connection.Config.Type (QueryParser(..), PaginationQueryParser(..), toResource)

import qualified Fixture as F


spec :: Spec
spec = do
  describe "refer epics" $ do
    it "first input" $ do
      let act = toQueryPart ReferEpicsInput F.owner F.repository Nothing

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    issues(first:100, states:OPEN, labels:[\"Epic\"]) {"
        , "      nodes {"
        , "        title"
        , "        number"
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
      let act = toQueryPart ReferEpicsInput F.owner F.repository F.cursor

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    issues(first:100, states:OPEN, labels:[\"Epic\"], after:\"abcd==\") {"
        , "      nodes {"
        , "        title"
        , "        number"
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
      let sut = "{\"data\":{\"repository\":{\"issues\":{\"nodes\":[{\"title\":\"registration module\",\"number\":1},{\"title\":\"user registration api\",\"number\":2}],\"pageInfo\":{\"endCursor\":\"abcd==\",\"hasNextPage\":true}}}}}"

      asEpics sut `shouldReturn` [F.epic1, F.epic2]
      parseHasNext ReferEpicsInput sut `shouldReturn` True
      parseEndCursor ReferEpicsInput sut `shouldBe` (Just "abcd==")

    it "last output" $ do
      let sut = "{\"data\":{\"repository\":{\"issues\":{\"nodes\":[],\"pageInfo\":{\"endCursor\":null,\"hasNextPage\":false}}}}}"

      asEpics sut `shouldReturn` []
      parseHasNext ReferEpicsInput sut `shouldReturn` False
      parseEndCursor ReferEpicsInput sut `shouldBe` Nothing

  describe "create issue" $ do
    it "input full parameters" $ do
      let sut = CreateIssueInput F.title1 F.body1 [F.label1, F.label2] [F.collaborator] (Just F.milestone1)

      toResource sut `shouldBe` "/issues"
      encode sut `shouldBe` "{\"assignees\":[\"suzuki-hoge\"],\"body\":\"post user data and write record.\",\"milestone\":1,\"labels\":[\"\229\174\159\232\163\133\",\"dev\"],\"title\":\"registration module\"}"

    it "input minimum parameters" $ do
      let sut = CreateIssueInput F.title1 F.body1 [] [] Nothing

      toResource sut `shouldBe` "/issues"
      encode sut `shouldBe` "{\"assignees\":[],\"body\":\"post user data and write record.\",\"labels\":[],\"title\":\"registration module\"}"

    it "output" $ do
      let act = asIssueNumber "{\"number\": 2}"

      act `shouldReturn` F.issueNumber

  describe "set pipeline" $ do
    it "input" $ do
      let sut = SetPipelineInput F.issueNumber F.pipeline1

      toResource sut `shouldBe` "/issues/2/moves"
      encode sut `shouldBe` "{\"pipeline_id\":\"abc123\",\"position\":\"bottom\"}"

  describe "set estimate" $ do
    it "input int" $ do
      let sut = SetEstimateInput F.issueNumber F.estimate1

      toResource sut `shouldBe` "/issues/2/estimate"
      encode sut `shouldBe` "{\"estimate\":3}"

    it "input double" $ do
      let sut = SetEstimateInput F.issueNumber F.estimate2

      toResource sut `shouldBe` "/issues/2/estimate"
      encode sut `shouldBe` "{\"estimate\":0.5}"

  describe "set epic" $ do
    it "input" $ do
      let sut = SetEpicInput F.issueNumber F.epicNumber1 F.repositoryId

      toResource sut `shouldBe` "/epics/1/update_issues"
      encode sut `shouldBe` "{\"add_issues\":[{\"issue_number\":2,\"repo_id\":131509978}]}"

  describe "convert to epic" $ do
    it "input" $ do
      let sut = ConvertToEpicInput F.issueNumber

      toResource sut `shouldBe` "/issues/2/convert_to_epic"
      encode sut `shouldBe` "{}"
