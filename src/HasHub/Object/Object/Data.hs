{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Data where


-- todo import list
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString) -- todo check LBS

import Data.Aeson (FromJSON(..), Value(Object), decode, (.:), (.:?), ToJSON(..), object, (.=))
import Data.Aeson.Types (Parser, parseMaybe)

import Data.List.Utils (replace)

import HasHub.Object.Milestone.Data
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data

import Data.List (intersectBy)

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


type RepositoryId = Int


data SetEpicInput = SetEpicInput IssueNumber RepositoryId
instance ToJSON SetEpicInput where
  toJSON (SetEpicInput (IssueNumber n) rid) = object $ [
      "add_issues" .= [object ["repo_id" .= rid, "issue_number" .= n]]
    ]


data SetPipelineInput = SetPipelineInput Pipeline
instance ToJSON SetPipelineInput where
  toJSON (SetPipelineInput (Pipeline id name)) = object $ [
      "pipeline_id" .= id
    , "position"    .= ("bottom" :: String)
    ]


newtype Estimate = Estimate Double deriving Eq
instance Show Estimate where
  show (Estimate v) = replace ".0" "" $ show v


data SetEstimateInput = SetEstimateInput Estimate -- todo instance ToResource ?
instance ToJSON SetEstimateInput where
  toJSON (SetEstimateInput (Estimate e)) = object $ [
      "estimate" .= e
    ]


data ConvertToEpicInput = ConvertToEpicInput
instance ToJSON ConvertToEpicInput where
  toJSON _ = object $ []


data EpicLinkNumber = EpicSharpNumber Int
                    | EpicQuestionNumber Int
                    deriving (Eq, Show)


data EpicNumber = EpicNumber Int deriving (Eq, Show)
instance FromJSON EpicNumber where
  parseJSON (Object v) = EpicNumber <$> (v .: "issue_number")


data Epic = Epic EpicNumber Title deriving Eq
instance Show Epic where
  show (Epic number title) = show number ++ " " ++ show title


parseInList :: LBS.ByteString -> Maybe [EpicNumber]
parseInList json = parseMaybe extract =<< decode json
  where
    extract :: Value -> Parser [EpicNumber]
    extract (Object v) = v .: "epic_issues"
    -- https://artyom.me/aeson#parsing-without-creating-extra-types


data LinkedEpic = LinkedEpic EpicLinkNumber EpicNumber deriving Show


data YamlObject = EpicYamlObject
                  EpicLinkNumber
                  Title
                  Body
                  [EpicLinkNumber]
                  (Maybe Estimate)           -- todo order by g -> z
                  (Maybe MilestoneTitle)
                  [Label]
                  [Collaborator]
                  (Maybe PipelineName)
            | IssueYamlObject
                  Title
                  Body
                  [EpicLinkNumber]
                  (Maybe Estimate)
                  (Maybe MilestoneTitle)
                  [Label]
                  [Collaborator]
                  (Maybe PipelineName)
            deriving (Eq, Show)



data YamlWrappedObject = YamlWrappedObject
                  (Maybe String)   -- epic-link-number
                  String           -- title
                  (Maybe String)   -- body
                  (Maybe [String]) -- epics
                  (Maybe Double)   -- estimate
                  (Maybe String)   -- milestone
                  (Maybe [String]) -- labels
                  (Maybe [String]) -- assignees
                  (Maybe String)   -- pipeline
               deriving Show
instance FromJSON YamlWrappedObject where
  parseJSON (Object v) = YamlWrappedObject <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "epics") <*> (v .:? "estimate") <*> (v .:? "milestone") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "pipeline")


fixEpicNumbers :: [LinkedEpic] -> [EpicLinkNumber] -> [EpicNumber]
fixEpicNumbers les elns = map (findAndConvert les) elns
  where
    findAndConvert :: [LinkedEpic] -> EpicLinkNumber -> EpicNumber
    findAndConvert les (EpicSharpNumber n) = EpicNumber n
    findAndConvert les eqn = mapped !! 0
      where
        filtered = filter (\(LinkedEpic eln en) -> eln == eqn) les
        mapped = map (\(LinkedEpic eln en) -> en) filtered
