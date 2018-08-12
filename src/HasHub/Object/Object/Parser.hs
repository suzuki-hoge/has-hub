{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Parser where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import HasHub.Object.Object.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Pipeline.Type
import HasHub.Connection.Type (RepositoryId)


data YamlWrappedObject2 = YamlWrappedObject2
                  (Maybe String)   -- epic-link-number
                  String           -- title
                  (Maybe String)   -- body
                  (Maybe String)   -- pipeline
                  (Maybe [String]) -- labels
                  (Maybe [String]) -- assignees
                  (Maybe String)   -- milestone
                  (Maybe Double)   -- estimate
                  (Maybe [String]) -- epics
               deriving Show
instance FromJSON YamlWrappedObject2 where
  parseJSON (Object v) = YamlWrappedObject2 <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "pipeline") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "estimate") <*> (v .:? "milestone") <*> (v .:? "epics")


data YamlObject2 = EpicYamlObject2
                  EpicLinkNumber2
                  Title2
                  Body2
                  (Maybe PipelineName2)
                  [Label2]
                  [Collaborator2]
                  (Maybe MilestoneTitle2)
                  (Maybe Estimate2)
                  [ParentEpicNumber2]
            | IssueYamlObject2
                  Title2
                  Body2
                  (Maybe PipelineName2)
                  [Label2]
                  [Collaborator2]
                  (Maybe MilestoneTitle2)
                  (Maybe Estimate2)
                  [ParentEpicNumber2]
            deriving (Eq, Show)
