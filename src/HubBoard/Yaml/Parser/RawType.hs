{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Yaml.Parser.RawType where

import           Data.Aeson

data Contents = Contents [RawEpic] RawMilestone RawPipelineName
instance FromJSON Contents where
    parseJSON (Object v) = Contents <$> (v .: "epics") <*> (v .: "milestone") <*> (v .: "default-pipeline")

data RawMilestone = RawMilestone (Maybe RawNewMilestone) (Maybe ExistingMilestone)
instance FromJSON RawMilestone where
    parseJSON (Object v) = RawMilestone <$> (v .:? "new-milestone") <*> (v .:? "existing-milestone")

type RawMilestoneTitle = String
type RawStartOn = String
type RawDueOn = String

data RawNewMilestone = RawNewMilestone RawMilestoneTitle RawStartOn RawDueOn
instance FromJSON RawNewMilestone where
    parseJSON (Object v) = RawNewMilestone <$> (v .: "title") <*> (v .: "start-on") <*> (v .: "due-on")

data ExistingMilestone = ExistingMilestone RawMilestoneTitle
instance FromJSON ExistingMilestone where
    parseJSON (Object v) = ExistingMilestone <$> (v .: "title")

type RawDefaultPipelineName = String

data RawDefaultPipeline = RawDefaultPipeline String
instance FromJSON RawDefaultPipeline where
    parseJSON (Object v) = RawDefaultPipeline <$> (v .: "name")

type RawTitle = String
type RawBody = String
type RawLabel = String
type RawAssignee = String
type RawPipelineName = String
type RawEstimate = Double

data RawEpic = RawEpic (Maybe RawNewEpic) (Maybe RawExistingEpic) (Maybe RawNoEpic)
instance FromJSON RawEpic where
    parseJSON (Object v) = RawEpic <$> (v .:? "new-epic") <*> (v .:? "existing-epic") <*> (v .:? "no-epic")

data RawNewEpic = RawNewEpic RawTitle (Maybe RawBody) (Maybe [RawLabel]) (Maybe [RawAssignee]) (Maybe RawPipelineName) (Maybe RawEstimate) (Maybe [RawIssue])
instance FromJSON RawNewEpic where
    parseJSON (Object v) = RawNewEpic <$> (v .: "title") <*> (v .:? "body") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "pipeline") <*> (v .:? "estimate") <*> (v .:? "issues")

type RawEpicNumber = Int

data RawExistingEpic = RawExistingEpic RawEpicNumber [RawIssue]
instance FromJSON RawExistingEpic where
    parseJSON (Object v) = RawExistingEpic <$> (v .: "number") <*> (v .: "issues")

data RawNoEpic = RawNoEpic [RawIssue]
instance FromJSON RawNoEpic where
    parseJSON (Object v) = RawNoEpic <$> (v .: "issues")

data RawIssue = RawIssue RawTitle (Maybe RawBody) (Maybe [RawLabel]) (Maybe [RawAssignee]) (Maybe RawPipelineName) (Maybe RawEstimate)
instance FromJSON RawIssue where
    parseJSON (Object v) = RawIssue <$> (v .: "title") <*> (v .:? "body") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "pipeline") <*> (v .:? "estimate")
