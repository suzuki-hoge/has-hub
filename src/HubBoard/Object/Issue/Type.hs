module HubBoard.Object.Issue.Type (IssueNumber(..), Issue(..)) where

import           HubBoard.Object.Label.Type
import           HubBoard.Object.Collaborator.Type
import           HubBoard.Object.Milestone.Type

type Title = String
type Body = String

data Issue = Issue Title Body [Label] [Collaborator] (Maybe MilestoneNumber)
newtype IssueNumber = IssueNumber Int