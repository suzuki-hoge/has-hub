{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Issue.Mapper
    ( create
    )
where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Internal as LBS
                                                ( ByteString )

import           HubBoard.Object.Issue.Type
import           HubBoard.Object.Label.Type
import           HubBoard.Object.Collaborator.Type
import           HubBoard.Object.Milestone.Type
import           HubBoard.Transfer.GitHubV4.Type
import           Data.Maybe
import           Data.Aeson.Types
import           HubBoard.Transfer.GitHubV3.Type

instance ToJSON Issue where
    toJSON (Issue title body labels collaborators milestoneNumber) =
        object $ ["title" .= title
        , "body" .= body
        , "labels" .= map (\(Label title) -> title) labels
        , "assignees" .= map (\(Collaborator name) -> name) collaborators
        ] ++ maybe [] (\(MilestoneNumber v) -> ["milestone" .= v]) milestoneNumber

instance FromJSON IssueNumber where
    parseJSON (Object v) = IssueNumber <$> (v .: "number")

create :: Issue -> PostMapper Issue IssueNumber
create issue = mkPostMapper "issues" issue (fromJust . decode)
