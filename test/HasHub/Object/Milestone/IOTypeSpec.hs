{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.IOTypeSpec where


import Test.Hspec

import Data.Aeson (encode)

import HasHub.Object.Milestone.IOType
import HasHub.Object.Milestone.Type

import HasHub.Connection.Config.Type (toResource, QueryParser(..), PaginationQueryParser(..))

import qualified Fixture as F


spec :: Spec
spec = do
  describe "refer git-hub-milestones" $ do
    it "first input" $ do
      let act = toQueryPart ReferGitHubMilestonesInput F.owner F.repository Nothing

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    milestones(first:100, states:OPEN) {"
        , "      nodes {"
        , "        number"
        , "        title"
        , "        dueOn"
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
      let act = toQueryPart ReferGitHubMilestonesInput F.owner F.repository F.cursor

      act `shouldBe` unlines [
          "query {"
        , "  repository(owner:\"suzuki-hoge\", name:\"has-hub-workspace\") {"
        , "    milestones(first:100, states:OPEN, after:\"abcd==\") {"
        , "      nodes {"
        , "        number"
        , "        title"
        , "        dueOn"
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
      let sut = "{\"data\":{\"repository\":{\"milestones\":{\"nodes\":[{\"number\": 1, \"title\": \"sprint 1\", \"dueOn\": \"2018-04-30T23:59:59Z\"}, {\"number\": 2, \"title\": \"sprint 2\", \"dueOn\": null}],\"pageInfo\":{\"endCursor\":\"abcd==\",\"hasNextPage\":true}}}}}"

      asGitHubMilestones sut `shouldReturn` [F.referGitHubMilestonesOutput1, F.referGitHubMilestonesOutput2]
      parseHasNext ReferGitHubMilestonesInput sut `shouldReturn` True
      parseEndCursor ReferGitHubMilestonesInput sut `shouldBe` (Just "abcd==")

    it "last output" $ do
      let sut = "{\"data\":{\"repository\":{\"milestones\":{\"nodes\":[],\"pageInfo\":{\"endCursor\":null,\"hasNextPage\":false}}}}}"

      asGitHubMilestones sut `shouldReturn` []
      parseHasNext ReferGitHubMilestonesInput sut `shouldReturn` False
      parseEndCursor ReferGitHubMilestonesInput sut `shouldBe` Nothing

  describe "refer start-on" $ do
    it "resource" $ do
      let act = toResource $ ReferStartOnInput F.milestoneNumber1

      act `shouldBe` "/milestones/1/start_date"

    it "output full parameters" $ do
      let act = asStartOn "{\"start_date\": \"2018-04-01T00:00:00Z\"}"

      act `shouldBe` F.startOn1

    it "output minimum parameters" $ do
      let act = asStartOn "{\"start_date\": null}"

      act `shouldBe` F.startOn2

  describe "create git-hub-milestone" $ do
    it "input full parameters" $ do
      let sut = F.createGitHubMilestoneInput1

      toResource sut `shouldBe` "/milestones"
      encode sut `shouldBe` "{\"due_on\":\"2018-04-30T23:59:59Z\",\"title\":\"sprint 1\"}"

    it "input minimum parameters" $ do
      let sut = F.createGitHubMilestoneInput2

      toResource sut `shouldBe` "/milestones"
      encode sut `shouldBe` "{\"title\":\"sprint 2\"}"

    it "output" $ do
      let act = asNumber "{\"number\":1}"

      act `shouldReturn` F.milestoneNumber1

  describe "create start-on" $ do
    it "input" $ do
      let sut = CreateStartOnInput F.milestoneNumber1 (StartOn "2018-04-01T00:00:00Z")

      toResource sut `shouldBe` "/milestones/1/start_date"
      encode sut `shouldBe` "{\"start_date\":\"2018-04-01T00:00:00Z\"}"
