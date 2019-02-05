{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Milestone.Mapper (
    refer
  , create
) where

import           HubBoard.Object.Milestone.Type
import           HubBoard.Fetcher

instance FromJSON MilestoneTitle where
    parseJSON (Object v) = MilestoneTitle <$> (v .: "title")

refer :: IO [MilestoneTitle]
refer = getFromGitHub toValue (pagenateWith "milestones") parse
  where
    toValue owner repository after = let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { milestones( first:100, states:OPEN%s ) { nodes { title }, pageInfo { hasNextPage, endCursor } } } }" owner repository after :: String
                                     in  object ["query" .= query]
    parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "milestones") >=> parseMaybe (.: "nodes"))

instance FromJSON MilestoneNumber where
    parseJSON (Object v) = MilestoneNumber <$> (v .: "number")

create :: Milestone -> IO MilestoneNumber
create (Milestone title startOn dueOn) = postToGitHub toResource value parse
  where
    toResource = printf "%s/%s/milestones"
    value = object ["title" .= title, "due_on" .= dueOn]
    parse = fromJust . decode
