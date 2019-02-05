module HubBoard.Object.Epic.Type (
    module HubBoard.Object.Collaborator.Type
  , module HubBoard.Object.Label.Type
  , module HubBoard.Object.Milestone.Type
  , module HubBoard.Object.Pipeline.Type
  , Epic(..)
  , EpicNumber(..)
) where

import           HubBoard.Object.Collaborator.Type
import           HubBoard.Object.Label.Type
import           HubBoard.Object.Milestone.Type
import           HubBoard.Object.Pipeline.Type
import           Text.Printf                    ( printf )

type Title = String
type Body = String
type Estimate = Double
data Epic = Epic Title Body [Label] (Maybe MilestoneNumber) Pipeline Estimate

newtype EpicNumber = EpicNumber Int
instance Show EpicNumber where
    show (EpicNumber number) = printf "EpicNumber(#%d)" number
