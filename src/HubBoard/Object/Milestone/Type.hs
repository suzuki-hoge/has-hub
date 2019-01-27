module HubBoard.Object.Milestone.Type (MilestoneTitle(..), MilestoneNumber(..), Milestone(..), StartOn(..)) where

import           Text.Printf                    ( printf )

data MilestoneTitle = MilestoneTitle String
instance Show MilestoneTitle where
    show (MilestoneTitle title) = printf "MilestoneTitle(%s)" title

type Title = String
type DueOn = String 

data Milestone = Milestone Title DueOn
newtype MilestoneNumber = MilestoneNumber Int

newtype StartOn = StartOn String