{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode, ToJSON(..), object, (.=))
import Data.Aeson.Types (parseMaybe)

import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import HasHub.Connection.Config.Type (ToResource(..), RepositoryId, QueryParser(..), PaginationQueryParser(..), mkAfter)

import HasHub.FixMe (asJust)


-- refer epics


data ReferEpicsInput = ReferEpicsInput

instance QueryParser ReferEpicsInput where
  toQueryPart _ owner repository cursor = unlines [
      "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    issues(first:100, states:OPEN, labels:[\"Epic\"]" ++ mkAfter cursor ++ ") {"
    , "      nodes {"
    , "        title"
    , "        number"
    , "      }"
    , "      pageInfo {"
    , "        hasNextPage"
    , "        endCursor"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]

instance PaginationQueryParser ReferEpicsInput where
  parseHasNext _ = asJust . parse
    where
      parse :: LBS.ByteString -> Maybe Bool
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "issues")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "hasNextPage")
  parseEndCursor _ = parse
    where
      parse :: LBS.ByteString -> Maybe String
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "issues")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "endCursor")


instance FromJSON Epic where
  parseJSON (Object v) = Epic <$> (EpicNumber <$> v .: "number") <*> (Title <$> v .: "title")


asEpics :: LBS.ByteString -> IO [Epic]
asEpics lbs = asJust $ parse lbs
  where
    parse :: LBS.ByteString -> Maybe [Epic]
    parse json = decode json
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: "issues")
        >>= parseMaybe (.: "nodes")


-- create issue


data CreateIssueInput = CreateIssueInput Title Body [Label] [Collaborator] (Maybe Milestone)

instance ToResource CreateIssueInput where
  toResource _ = "/issues"

instance ToJSON CreateIssueInput where
  toJSON (CreateIssueInput (Title t) (Body b) labels collaborators milestone) = object $ [
      "title"     .= t
    , "body"      .= b
    , "assignees" .= map (\(Collaborator c) -> c) collaborators
    , "labels"    .= map (\(Label l) -> l) labels
    ] ++ maybe [] (\(Milestone (MilestoneNumber n) _ _ _) -> ["milestone" .= n]) milestone


instance FromJSON IssueNumber where
  parseJSON (Object v) = IssueNumber <$> (v .: "number")

asIssueNumber :: LBS.ByteString -> IO IssueNumber
asIssueNumber lbs = asJust $ decode lbs


-- set pipeline


data SetPipelineInput = SetPipelineInput IssueNumber Pipeline

instance ToResource SetPipelineInput where
  toResource (SetPipelineInput (IssueNumber n) _) = "/issues/" ++ show n ++ "/moves"

instance ToJSON SetPipelineInput where
  toJSON (SetPipelineInput _ (Pipeline (PipelineId i) _)) = object [
      "pipeline_id" .= i
    , "position"    .= ("bottom" :: String)
    ]


-- set estimate


data SetEstimateInput = SetEstimateInput IssueNumber Estimate

instance ToResource SetEstimateInput where
  toResource (SetEstimateInput (IssueNumber n) _) = "/issues/" ++ show n ++ "/estimate"

instance ToJSON SetEstimateInput where
  toJSON (SetEstimateInput _ (Estimate e)) = object [
      "estimate" .= e
    ]


-- set epic


data SetEpicInput = SetEpicInput IssueNumber EpicNumber RepositoryId

instance ToResource SetEpicInput where
  toResource (SetEpicInput _ (EpicNumber n) _) = "/epics/" ++ show n ++ "/update_issues"

instance ToJSON SetEpicInput where
  toJSON (SetEpicInput (IssueNumber n) _ rid) = object [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


-- convert to epic


newtype ConvertToEpicInput = ConvertToEpicInput IssueNumber

instance ToResource ConvertToEpicInput where
  toResource (ConvertToEpicInput (IssueNumber n)) = "/issues/" ++ show n ++ "/convert_to_epic"

instance ToJSON ConvertToEpicInput where
  toJSON _ = object []
