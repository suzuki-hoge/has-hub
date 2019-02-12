{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Yaml.ParserSpec where


import           Test.Hspec

import           HubBoard.Yaml.Parser

import           HubBoard.Yaml.Parser.RawType ( RawMilestone(..) )

pipeline1 = Pipeline "1" "backlog"
pipeline2 = Pipeline "2" "sprint backlog"
milestoneNumber1 = Just $ MilestoneNumber 1
milestoneNumber2 = Just $ MilestoneNumber 2
noMilestoneNumber = Nothing

spec :: Spec
spec = do
  describe "epic patterns" $ do
    it "new epic" $ do
      epics <- parse' "test/yamls/epic-patterns/new-epic.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NewEpic "epic title 1" "epic body 1" [Label "dev"] [Collaborator "suzuki-hoge"] noMilestoneNumber pipeline1 5.0 [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        , NewEpic "epic title 2" "" [] [] noMilestoneNumber pipeline2 0.0 [

            ]
        ]

    it "existing epic" $ do
      epics <- parse' "test/yamls/epic-patterns/existing-epic.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          ExistingEpic (EpicNumber 1) [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

    it "no epic" $ do
      epics <- parse' "test/yamls/epic-patterns/no-epic.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

    it "combination" $ do
      epics <- parse' "test/yamls/epic-patterns/combination.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NewEpic "epic title 1" "" [] [] noMilestoneNumber pipeline2 0.0 [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        , NewEpic "epic title 2" "" [] [] noMilestoneNumber pipeline2 0.0 [

            ]
        , ExistingEpic (EpicNumber 1) [
              Issue "issue title 2" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        , NoEpic [
              Issue "issue title 3" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

  describe "issue patterns" $ do
    it "full attributes, explicit default pipeline, int estimate" $ do
      epics <- parse' "test/yamls/issue-patterns/full-attributes.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "issue body 1" [Label "setup"] [Collaborator "suzuki-hoge"] noMilestoneNumber pipeline2 3.0
            ]
        ]

    it "empty attributes" $ do
      epics <- parse' "test/yamls/issue-patterns/empty-attributes.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

    it "minimum attributes" $ do
      epics <- parse' "test/yamls/issue-patterns/minimum-attributes.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

    it "multi lines body, non default pipeline, double estimate" $ do
      epics <- parse' "test/yamls/issue-patterns/multi-lines-body.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "# issue body 4\n+ [ ] task 1\n+ [ ] task 2\n" [] [] noMilestoneNumber pipeline1 0.5
            ]
        ]

  describe "milestone patterns" $ do
    it "new milestones" $ do
      epics <- parse' "test/yamls/milestone-patterns/new-milestone.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] milestoneNumber2 pipeline2 0.0
            ]
        ]

    it "existing milestones" $ do
      epics <- parse' "test/yamls/milestone-patterns/existing-milestone.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] milestoneNumber1 pipeline2 0.0
            ]
        ]

    it "no milestones" $ do
      epics <- parse' "test/yamls/milestone-patterns/no-milestone.yaml" setupMilestoneMock fetchPipelinesMock validateAllMock

      epics `shouldBe` Right [
          NoEpic [
              Issue "issue title 1" "" [] [] noMilestoneNumber pipeline2 0.0
            ]
        ]

setupMilestoneMock (RawMilestone Nothing Nothing) = return noMilestoneNumber
setupMilestoneMock (RawMilestone _ Nothing) = return milestoneNumber2
setupMilestoneMock (RawMilestone Nothing _) = return milestoneNumber1

fetchPipelinesMock = return [pipeline1, pipeline2]

validateAllMock _ _ _  = return []