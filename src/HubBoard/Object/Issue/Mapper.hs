{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Issue.Mapper (
    create
) where

import           HubBoard.Object.Issue.Type
import           HubBoard.Fetcher

instance FromJSON IssueNumber where
    parseJSON (Object v) = IssueNumber <$> (v .: "number")

create :: Issue -> IO IssueNumber
create (Issue title body labels collaborators milestoneNumber pipeline estimate) = do
    issueNumber <- createIssue title body labels collaborators milestoneNumber

    setPipeline issueNumber pipeline
    setEstimate issueNumber estimate

    guide issueNumber title

    return issueNumber
      where
        createIssue title body labels collaborators milestoneNumber = postToGitHub toResource value parse
          where
            toResource = printf "%s/%s/issues"
            value = object $ ["title" .= title, "body" .= body, "labels" .= ls, "assignees" .= cs] ++ mn milestoneNumber
              where
                ls = map (\(Label name) -> name) labels
                cs = map (\(Collaborator name) -> name) collaborators
                mn Nothing = []
                mn (Just (MilestoneNumber milestoneNumber)) = ["milestone" .= milestoneNumber]
            parse = fromJust . decode

        setPipeline (IssueNumber issueNumber) (Pipeline pipelineId _) = updateZenHub toResource value "POST" parse
          where
            toResource rid = printf "%s/issues/%d/moves" rid issueNumber
            value = object ["pipeline_id" .= pipelineId, "position" .= ("bottom" :: String)]
            parse = const ()

        setEstimate (IssueNumber issueNumber) estimate = updateZenHub toResource value "PUT" parse
          where
            toResource rid = printf "%s/issues/%d/estimate" rid issueNumber
            value = object ["estimate" .= estimate]
            parse = const ()

        guide (IssueNumber issueNumber) title = putStrLn $ printf "  [#%d] %s" issueNumber title