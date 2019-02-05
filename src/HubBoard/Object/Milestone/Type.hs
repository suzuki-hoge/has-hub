module HubBoard.Object.Milestone.Type (
    MilestoneTitle(..)
  , Milestone(..)
  , MilestoneNumber(..)
) where

import           Text.Printf                    ( printf )

newtype MilestoneTitle = MilestoneTitle String
instance Show MilestoneTitle where
    show (MilestoneTitle title) = printf "MilestoneTitle(%s)" title

type Title = String
type StartOn = String
type DueOn = String
data Milestone = Milestone Title StartOn DueOn

newtype MilestoneNumber = MilestoneNumber Int
