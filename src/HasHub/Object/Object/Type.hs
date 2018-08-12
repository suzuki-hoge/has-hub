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


data IssueNumber2 = IssueNumber2 Int deriving (Eq, Show)
instance FromJSON IssueNumber2 where
  parseJSON (Object v) = IssueNumber2 <$> (v .: "number")


newtype Title2 = Title2 String deriving (Eq, Show)


newtype Body2 = Body2 String deriving (Eq, Show)


-- todo definition order p -> l -> a...
data CreateIssueInput2 = CreateIssueInput2 Title2 Body2 (Maybe Milestone2) [Collaborator2] [Label2] -- todo order
instance ToJSON CreateIssueInput2 where
  toJSON (CreateIssueInput2 (Title2 t) (Body2 b) mm cs ls) = object $ [
      "title"     .= t
    , "body"      .= b
    , "assignees" .= map (\(Collaborator2 c) -> c) cs
    , "labels"    .= map (\(Label2 l) -> l) ls
    ] ++ maybe [] (\(Milestone2 (MilestoneNumber2 n) _ _ _) -> ["milestone" .= n]) mm


data LinkedEpic2 = LinkedEpic2 EpicLinkNumber2 EpicNumber2 deriving Show


newtype EpicLinkNumber2 = EpicLinkNumber2 String deriving (Eq, Ord, Show)


data ParentEpicNumber2 = SharpEpicNumber2 String | QuestionEpicNumber2 String deriving (Eq, Ord, Show)


data EpicNumber2 = EpicNumber2 Int deriving (Eq, Show)
instance FromJSON EpicNumber2 where
  parseJSON (Object v) = EpicNumber2 <$> (v .: "issue_number")


decodeJust :: LBS.ByteString -> [EpicNumber2]
decodeJust = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [EpicNumber2]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "epic_issues")


decodeJust' :: LBS.ByteString -> IssueNumber2
decodeJust' = fromJust . decode


newtype Estimate2 = Estimate2 Double deriving (Eq, Show)


data SetEpicInput2 = SetEpicInput2 IssueNumber2 RepositoryId
instance ToJSON SetEpicInput2 where
  toJSON (SetEpicInput2 (IssueNumber2 n) rid) = object $ [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


data SetPipelineInput2 = SetPipelineInput2 Pipeline2
instance ToJSON SetPipelineInput2 where
  toJSON (SetPipelineInput2 (Pipeline2 (PipelineId2 i) _)) = object $ [
      "pipeline_id" .= i
    , "position"    .= ("bottom" :: String)
    ]


data SetEstimateInput2 = SetEstimateInput2 Estimate2
instance ToJSON SetEstimateInput2 where
  toJSON (SetEstimateInput2 (Estimate2 e)) = object $ [
      "estimate" .= e
    ]


data ConvertToEpicInput2 = ConvertToEpicInput2
instance ToJSON ConvertToEpicInput2 where
  toJSON _ = object $ []
