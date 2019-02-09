module HubBoard.Object.Issue.Type (
    module HubBoard.Object.Collaborator.Type
  , module HubBoard.Object.Label.Type
  , module HubBoard.Object.Milestone.Type
  , module HubBoard.Object.Pipeline.Type
  , Issue(..)
  , IssueNumber(..)
) where

import           HubBoard.Object.Collaborator.Type
import           HubBoard.Object.Label.Type
import           HubBoard.Object.Milestone.Type
import           HubBoard.Object.Pipeline.Type

type Title = String
type Body = String
type Estimate = Double
data Issue = Issue Title Body [Label] [Collaborator] (Maybe MilestoneNumber) Pipeline Estimate deriving (Show, Eq)

data IssueNumber = IssueNumber Int
