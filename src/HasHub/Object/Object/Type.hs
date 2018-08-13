{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode, ToJSON(..), object, (.=))
import Data.Aeson.Types (parseMaybe)

import Data.Maybe (fromJust)

import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import HasHub.Connection.Type (RepositoryId)

import HasHub.Connection.Type (ToResource(..))


data ReferEpicResourcer = ReferEpicResourcer
instance ToResource ReferEpicResourcer where
  toResource _ = "/epics"


newtype EpicNumber = EpicNumber Int deriving (Eq, Show)
instance FromJSON EpicNumber where
  parseJSON (Object v) = EpicNumber <$> (v .: "issue_number")


decodeJust :: LBS.ByteString -> [EpicNumber]
decodeJust = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [EpicNumber]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "epic_issues")


newtype EpicLinkNumber = EpicLinkNumber String deriving (Eq, Ord, Show)


data ParentEpicNumber = SharpEpicNumber String | QuestionEpicNumber String deriving (Eq, Ord, Show)


data LinkedEpic = LinkedEpic EpicLinkNumber EpicNumber deriving Show


newtype IssueNumber = IssueNumber Int deriving (Eq, Show)
instance FromJSON IssueNumber where
  parseJSON (Object v) = IssueNumber <$> (v .: "number")


decodeJust' :: LBS.ByteString -> IssueNumber -- todo name?
decodeJust' = fromJust . decode


newtype Title = Title String deriving (Eq, Show)


newtype Body = Body String deriving (Eq, Show)


newtype Estimate = Estimate Double deriving (Eq, Show)


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


data SetPipelineInput = SetPipelineInput IssueNumber Pipeline
instance ToResource SetPipelineInput where
  toResource (SetPipelineInput (IssueNumber n) _) = "/issues/" ++ show n ++ "/moves"
instance ToJSON SetPipelineInput where
  toJSON (SetPipelineInput _ (Pipeline (PipelineId i) _)) = object [
      "pipeline_id" .= i
    , "position"    .= ("bottom" :: String)
    ]


data SetEstimateInput = SetEstimateInput IssueNumber Estimate
instance ToResource SetEstimateInput where
  toResource (SetEstimateInput (IssueNumber n) _) = "/issues/" ++ show n ++ "/estimate"
instance ToJSON SetEstimateInput where
  toJSON (SetEstimateInput _ (Estimate e)) = object [
      "estimate" .= e
    ]


data SetEpicInput = SetEpicInput IssueNumber EpicNumber RepositoryId
instance ToResource SetEpicInput where
  toResource (SetEpicInput _ (EpicNumber n) _) = "/epics/" ++ show n ++ "/update_issues"
instance ToJSON SetEpicInput where
  toJSON (SetEpicInput (IssueNumber n) _ rid) = object [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


data ConvertToEpicInput = ConvertToEpicInput IssueNumber
instance ToResource ConvertToEpicInput where
  toResource (ConvertToEpicInput (IssueNumber n)) = "/issues/" ++ show n ++ "/convert_to_epic"
instance ToJSON ConvertToEpicInput where
  toJSON _ = object []


_epicNumber :: IssueNumber -> EpicNumber
_epicNumber (IssueNumber n) = EpicNumber n
