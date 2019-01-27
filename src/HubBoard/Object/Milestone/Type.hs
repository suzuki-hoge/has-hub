module HubBoard.Object.Milestone.Type (Milestone(..)) where

import           Text.Printf                    ( printf )

data Milestone = Milestone Int String
instance Show Milestone where
    show (Milestone _ title) = printf "Milestone(%s)" title
