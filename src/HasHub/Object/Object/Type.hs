{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode, ToJSON(..), object, (.=))
import Data.Aeson.Types (Parser, parseMaybe)

import Data.Maybe (fromJust)

import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Pipeline.Type
import HasHub.Connection.Type (RepositoryId)


data IssueNumber = IssueNumber Int deriving (Eq, Show)
instance FromJSON IssueNumber where
  parseJSON (Object v) = IssueNumber <$> (v .: "number")


newtype Title = Title String deriving (Eq, Show)


newtype Body = Body String deriving (Eq, Show)


data CreateIssueInput = CreateIssueInput Title Body (Maybe Milestone) [Collaborator] [Label]
instance ToJSON CreateIssueInput where
  toJSON (CreateIssueInput (Title t) (Body b) mm cs ls) = object $ [
      "title"     .= t
    , "body"      .= b
    , "assignees" .= map (\(Collaborator c) -> c) cs
    , "labels"    .= map (\(Label l) -> l) ls
    ] ++ maybe [] (\(Milestone (MilestoneNumber n) _ _ _) -> ["milestone" .= n]) mm


data LinkedEpic = LinkedEpic EpicLinkNumber EpicNumber deriving Show


newtype EpicLinkNumber = EpicLinkNumber String deriving (Eq, Ord, Show)


data ParentEpicNumber = SharpEpicNumber String | QuestionEpicNumber String deriving (Eq, Ord, Show)


data EpicNumber = EpicNumber Int deriving (Eq, Show)
instance FromJSON EpicNumber where
  parseJSON (Object v) = EpicNumber <$> (v .: "issue_number")


decodeJust :: LBS.ByteString -> [EpicNumber]
decodeJust = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [EpicNumber]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "epic_issues")


decodeJust' :: LBS.ByteString -> IssueNumber -- todo name?
decodeJust' = fromJust . decode


newtype Estimate = Estimate Double deriving (Eq, Show)


data SetPipelineInput = SetPipelineInput Pipeline
instance ToJSON SetPipelineInput where
  toJSON (SetPipelineInput (Pipeline (PipelineId i) _)) = object $ [
      "pipeline_id" .= i
    , "position"    .= ("bottom" :: String)
    ]


data SetEstimateInput = SetEstimateInput Estimate
instance ToJSON SetEstimateInput where
  toJSON (SetEstimateInput (Estimate e)) = object $ [
      "estimate" .= e
    ]


data SetEpicInput = SetEpicInput IssueNumber RepositoryId
instance ToJSON SetEpicInput where
  toJSON (SetEpicInput (IssueNumber n) rid) = object $ [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


data ConvertToEpicInput = ConvertToEpicInput
instance ToJSON ConvertToEpicInput where
  toJSON _ = object $ []
