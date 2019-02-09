module HubBoard.Object.Milestone.Type (
    Milestone(..)
  , MilestoneNumber(..)
  , MilestoneMaterials
) where

type Number = Int
type Title = String

data Milestone = Milestone Number Title
instance Show Milestone where
    show (Milestone number title) = title

type MilestoneMaterials = (Title, StartOn, DueOn)

type StartOn = String
type DueOn = String

newtype MilestoneNumber = MilestoneNumber Int deriving (Show, Eq)
