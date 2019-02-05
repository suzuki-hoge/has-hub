{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Issue.Mapper (
    create
) where

import           HubBoard.Object.Issue.Type
import           HubBoard.Fetcher

instance FromJSON IssueNumber where
    parseJSON (Object v) = IssueNumber <$> (v .: "number")

create :: Issue -> IO IssueNumber
create (Issue title body labels collaborators milestoneNumber pipeline epicNumber estimate) = do
    issueNumber <- createIssue title body labels collaborators milestoneNumber

    mapM_ (setEpic issueNumber) epicNumber
    setPipeline issueNumber pipeline
    setEstimate issueNumber estimate

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

        setEpic (IssueNumber issueNumber) (EpicNumber epicNumber) = do
            rid <- read <$> getRepositoryId
            updateZenHub toResource (value rid) "POST" parse
          where
            toResource rid = printf "%s/epics/%d/update_issues" rid epicNumber
            value = (\rid -> object ["add_issues" .= [object ["repo_id" .= rid, "issue_number" .= issueNumber]]]) :: Int -> Value
            parse = const ()

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
