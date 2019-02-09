module HubBoard.Object.Epic.Type (
    module HubBoard.Object.Collaborator.Type
  , module HubBoard.Object.Label.Type
  , module HubBoard.Object.Milestone.Type
  , module HubBoard.Object.Pipeline.Type
  , Epic(..)
  , EpicNumber(..)
) where

import           HubBoard.Object.Collaborator.Type
import           HubBoard.Object.Issue.Type
import           HubBoard.Object.Label.Type
import           HubBoard.Object.Milestone.Type
import           HubBoard.Object.Pipeline.Type

type Title = String
type Body = String
type Estimate = Double
data Epic = NewEpic Title Body [Label] [Collaborator] (Maybe MilestoneNumber) Pipeline Estimate [Issue]
          | ExistingEpic EpicNumber [Issue]
          | NoEpic [Issue]
          deriving (Show, Eq)

newtype EpicNumber = EpicNumber Int deriving Eq
instance Show EpicNumber where
    show (EpicNumber number) = "#" ++ show number
