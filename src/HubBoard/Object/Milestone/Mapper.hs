{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Milestone.Mapper
    ( refer
    , create
    , setStartOn
    )
where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Internal as LBS
                                                ( ByteString )

import           HubBoard.Object.Milestone.Type
import           HubBoard.Transfer.GitHubV4.Type as V4
import           Data.Maybe
import           Data.Aeson.Types
import           HubBoard.Transfer.GitHubV3.Type
import qualified HubBoard.Transfer.ZenHub.Type as Z

import           Text.Printf                    ( printf )

instance FromJSON MilestoneTitle where
    parseJSON (Object v) = MilestoneTitle <$> (v .: "title")

refer :: GetMapper MilestoneTitle
refer = mkGetMapper "milestones" "first:100, states:OPEN" "number, title"

instance ToJSON Milestone where
    toJSON (Milestone title dueOn) =
        object $ ["title" .= title, "due_on" .= dueOn]

instance FromJSON MilestoneNumber where
    parseJSON (Object v) = MilestoneNumber <$> (v .: "number")

create :: Milestone -> PostMapper Milestone MilestoneNumber
create milestone = mkPostMapper "milestones" milestone (fromJust . decode)

instance ToJSON StartOn where
    toJSON (StartOn startOn) = object $ ["start_date" .= startOn]

setStartOn :: MilestoneNumber -> StartOn -> Z.UpdateMapper StartOn
setStartOn (MilestoneNumber number) = Z.mkPostMapper (\rid -> printf "%s/milestones/%d/start_date" rid number)