{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Milestone.Mapper (
    refer
  , create
) where

import           HubBoard.Object.Milestone.Type
import           HubBoard.Fetcher

instance FromJSON Milestone where
    parseJSON (Object v) = Milestone <$> (v .: "number") <*> (v .: "title")

refer :: IO [Milestone]
refer = getFromGitHub toValue (pagenateWith "milestones") parse
  where
    toValue owner repository after = let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { milestones( first:100, states:OPEN%s ) { nodes { number, title }, pageInfo { hasNextPage, endCursor } } } }" owner repository after :: String
                                     in  object ["query" .= query]
    parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "milestones") >=> parseMaybe (.: "nodes"))

instance FromJSON MilestoneNumber where
    parseJSON (Object v) = MilestoneNumber <$> (v .: "number")

create :: MilestoneMaterials -> IO MilestoneNumber
create (title, startOn, dueOn) = postToGitHub toResource value parse
  where
    toResource = printf "%s/%s/milestones"
    value = object ["title" .= title, "due_on" .= dueOn]
    parse = fromJust . decode
