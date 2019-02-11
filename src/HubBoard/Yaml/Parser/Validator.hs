{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Yaml.Parser.Validator (
    validateAll
  , ErrorMessage
) where

import           Data.List                    ( nub, (\\) )

import qualified HubBoard.Fetcher             as F
import           HubBoard.Yaml.Parser.RawType
import           HubBoard.Object.Epic         as E
import qualified HubBoard.Object.Collaborator as C
import qualified HubBoard.Object.Label        as L
import qualified HubBoard.Object.Milestone    as M
import qualified HubBoard.Object.Pipeline     as P

type ErrorMessage = String

validateAll :: [RawEpic] -> RawMilestone -> RawDefaultPipelineName -> IO [ErrorMessage]
validateAll rawEpics rawMilestone rawDefaultPipelineName = do
    putStrLn "\nvalidate..."

    let rawLabels          = nub $ concatMap epicLabels        rawEpics
    let rawCollaborators   = nub $ concatMap epicCollaborators rawEpics
    let rawPipelineNames   = nub $ concatMap epicPipelineNames rawEpics
    let rawEpicNumbers     = nub $ concatMap epicNumbers       rawEpics
    let rawMilestoneTitles =                 milestoneTitles   rawMilestone
    
    putStrLn "  refer Labels"
    invalidLabels         <- map ("  no such label      : " ++ ) .            (rawLabels \\)          . (map (\(Label        v) -> v)) <$> L.refer
    putStrLn "  refer Assignees"
    invalidCollaborators  <- map ("  no such assignee   : " ++ ) .            (rawCollaborators \\)   . (map (\(Collaborator v) -> v)) <$> C.refer
    putStrLn "  refer Pipelines"
    invalidPipelineNames  <- map ("  no such pipeline   : " ++ ) .            (rawPipelineNames \\)   . (map (\(Pipeline _   v) -> v)) <$> P.refer
    putStrLn "  refer Epics"
    invalidEpicNumbers    <- map ("  no such epic number: " ++ ) . map show . (rawEpicNumbers \\)     . (map (\(EpicNumber   v) -> v)) <$> E.refer
    putStrLn "  refer Milestones"
    invalidMilestoneTitle <- map ("  no such milestone  : " ++ ) .            (rawMilestoneTitles \\) . (map (\(Milestone _  v) -> v)) <$> M.refer

    return $ invalidLabels ++ invalidCollaborators ++ invalidPipelineNames ++ invalidEpicNumbers ++ invalidMilestoneTitle

epicLabels :: RawEpic -> [RawLabel]
epicLabels (RawEpic (Just (RawNewEpic _ _ rawLabels _ _ _ rawIssues)) _ _) = rawLabels ? [] ++ concatMap issueLabels (rawIssues ? [])
epicLabels (RawEpic _ (Just (RawExistingEpic _ rawIssues)) _) = concatMap issueLabels rawIssues
epicLabels (RawEpic _ _ (Just (RawNoEpic rawIssues))) = concatMap issueLabels rawIssues

issueLabels :: RawIssue -> [RawLabel]
issueLabels (RawIssue _ _ rawLabels _ _ _) = rawLabels ? []

epicCollaborators :: RawEpic -> [RawAssignee]
epicCollaborators (RawEpic (Just (RawNewEpic _ _ _ rawCollaborators _ _ rawIssues)) _ _) = rawCollaborators ? [] ++ concatMap issueCollaborators (rawIssues ? [])
epicCollaborators (RawEpic _ (Just (RawExistingEpic _ rawIssues)) _) = concatMap issueCollaborators rawIssues
epicCollaborators (RawEpic _ _ (Just (RawNoEpic rawIssues))) = concatMap issueCollaborators rawIssues

issueCollaborators :: RawIssue -> [RawAssignee]
issueCollaborators (RawIssue _ _ _ rawCollaborators _ _) = rawCollaborators ? []

epicPipelineNames :: RawEpic -> [RawPipelineName]
epicPipelineNames (RawEpic (Just (RawNewEpic _ _ _ _ rawPipelineName _ rawIssues)) _ _) = asList rawPipelineName ++ concatMap issuePipelineNames (rawIssues ? [])
epicPipelineNames (RawEpic _ (Just (RawExistingEpic _ rawIssues)) _) = concatMap issuePipelineNames rawIssues
epicPipelineNames (RawEpic _ _ (Just (RawNoEpic rawIssues))) = concatMap issuePipelineNames rawIssues

issuePipelineNames :: RawIssue -> [RawPipelineName]
issuePipelineNames (RawIssue _ _ _ _ rawPipelineName _) = asList rawPipelineName

epicNumbers :: RawEpic -> [RawEpicNumber]
epicNumbers (RawEpic _ (Just (RawExistingEpic epicNumber _)) _) = [epicNumber]
epicNumbers _ = []

milestoneTitles :: RawMilestone -> [RawMilestoneTitle]
milestoneTitles (RawMilestone _ (Just (ExistingMilestone title))) = [title]
milestoneTitles _ = []

(?) :: Maybe a -> a -> a
Nothing  ? def = def
(Just v) ? _   = v

asList :: Maybe a -> [a]
asList Nothing  = []
asList (Just v) = [v]