{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Issue.Data where

import Data.Aeson (FromJSON(..), Value(Object), ToJSON(..), (.:), (.:?), (.=), object)

import Data.List.Utils (replace)

import qualified HasHub.Object.Milestone.Data as M (Milestone(..), Number(..))
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import qualified HasHub.Object.Epic.Data as E
import qualified HasHub.Client.Data as Cl


data Number = Number Int deriving (Eq)
instance FromJSON Number where
  parseJSON (Object v) = Number <$> (v .: "number")
instance Show Number where
  show (Number v) = "#" ++ show v


newtype Title = Title String
instance Show Title where
  show (Title v) = v


newtype Body = Body String


data GitHubInput = GitHubInput Title Body (Maybe M.Number) [Collaborator] [Label]
instance ToJSON GitHubInput where
  toJSON (GitHubInput (Title t) (Body b) mm cs ls) = object $ [
      "title"     .= t
    , "body"      .= b
    , "assignees" .= map (\(Collaborator name) -> name) cs
    , "labels"    .= map (\(Label name) -> name) ls
    ] ++ maybe [] (\(M.Number number) -> ["milestone" .= number]) mm


data EpicInput = EpicInput Number Cl.RepositoryId
instance ToJSON EpicInput where
  toJSON (EpicInput (Number n) rid) = object $ [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


newtype PipelineId = PipelineId String


newtype PipelineName = PipelineName String
instance Show PipelineName where
  show (PipelineName v) = v


data Pipeline = Pipeline PipelineId PipelineName


data PipelineInput = PipelineInput PipelineId
instance ToJSON PipelineInput where
  toJSON (PipelineInput (PipelineId id)) = object $ [
      "pipeline_id" .= id
    , "position"    .= ("bottom" :: String)
    ]


newtype Estimate = Estimate Double
instance Show Estimate where
  show (Estimate v) = replace ".0" "" $ show v


data EstimateInput = EstimateInput Estimate
instance ToJSON EstimateInput where
  toJSON (EstimateInput (Estimate e)) = object $ [
      "estimate" .= e
    ]
