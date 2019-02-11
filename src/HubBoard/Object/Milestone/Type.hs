module HubBoard.Object.Milestone.Type (
    Milestone(..)
  , MilestoneNumber(..)
  , MilestoneMaterials
) where

type Number = Int
type Title = String

data Milestone = Milestone Number Title deriving (Show, Eq)

type MilestoneMaterials = (Title, StartOn, DueOn)

type StartOn = String
type DueOn = String

newtype MilestoneNumber = MilestoneNumber Int deriving (Show, Eq)
