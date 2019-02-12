{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Yaml.Parser.Converter (
    module HubBoard.Object.Epic.Type
  , module HubBoard.Object.Issue.Type
  , asEpics
) where

import           HubBoard.Yaml.Parser.RawType
import           HubBoard.Object.Issue.Type
import           HubBoard.Object.Epic.Type

asEpics :: [RawEpic] -> Maybe MilestoneNumber -> [Pipeline] -> RawDefaultPipelineName-> [Epic]
asEpics rawEpics milestoneNumber pipelines rawDefaultPipelineName = map toEpic rawEpics
  where
    toEpic :: RawEpic -> Epic
    toEpic (RawEpic (Just (RawNewEpic rawTitle rawBody rawLabels rawCollaborators rawPipelineName rawEstimate rawIssues)) Nothing Nothing) =
        NewEpic
            rawTitle
            (rawBody ? "")
            (rawLabels ?? Label)
            (rawCollaborators ?? Collaborator)
            milestoneNumber
            (pipelines ?- (rawPipelineName ? rawDefaultPipelineName))
            (rawEstimate ? 0)
            (map toIssue $ rawIssues ? [])
    toEpic (RawEpic Nothing (Just (RawExistingEpic epicNumber rawIssues)) Nothing) =
        ExistingEpic
            (EpicNumber epicNumber)
            (map toIssue rawIssues)
    toEpic (RawEpic Nothing Nothing (Just (RawNoEpic rawIssues))) =
        NoEpic
            (map toIssue rawIssues)

    toIssue :: RawIssue -> Issue
    toIssue (RawIssue rawTitle rawBody rawLabels rawCollaborators rawPipelineName rawEstimate) = 
        Issue
            rawTitle
            (rawBody ? "")
            (rawLabels ?? Label)
            (rawCollaborators ?? Collaborator)
            milestoneNumber
            (pipelines ?- (rawPipelineName ? rawDefaultPipelineName))
            (rawEstimate ? 0)

(?) :: Maybe a -> a -> a
Nothing  ? def = def
(Just v) ? _   = v

(??) :: Maybe [a] -> (a -> b) -> [b]
Nothing   ?? _ = []
(Just vs) ?? f = map f vs

(?-) :: [Pipeline] -> RawPipelineName -> Pipeline
ps ?- name = filter (\(Pipeline _ n) -> n == name) ps !! 0