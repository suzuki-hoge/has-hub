{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Parser
(
  readObjects
, YamlObject(..)
, YamlWrappedObject(..)
, module HasHub.FixMe
)
where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import HasHub.Yaml.Reader (readYaml)

import HasHub.Object.Object.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Pipeline.Type
import HasHub.Connection.Type (RepositoryId)

import HasHub.FixMe (Validation(..), Error)


data YamlObject = EpicYamlObject
                  EpicLinkNumber
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [ParentEpicNumber]
            | IssueYamlObject
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [ParentEpicNumber]
            deriving (Eq, Show)


data YamlWrappedObject = YamlWrappedObject
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
instance FromJSON YamlWrappedObject where
  parseJSON (Object v) = YamlWrappedObject <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "pipeline") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "milestone") <*> (v .:? "estimate") <*> (v .:? "epics")


readObjects :: FilePath -> IO (Validation [Error] [YamlObject])
readObjects = readYaml mapping
  where
    mapping :: YamlWrappedObject -> YamlObject
    mapping (YamlWrappedObject meln t mb mpn ls cs mmt me es) = createEither meln (Title t) (Body <?> mb) (PipelineName <$> mpn) (Label <??> ls) (Collaborator <??> cs) (MilestoneTitle <$> mmt) (Estimate <$> me) (toParent <??> es)
      where
        createEither (Just eln) = EpicYamlObject (EpicLinkNumber eln)
        createEither Nothing    = IssueYamlObject

        (<?>) :: (String -> a) -> Maybe String -> a
        f <?> Nothing = f ""
        f <?> (Just xs) = f xs


        (<??>) :: (a -> b) -> Maybe [a] -> [b]
        f <??> Nothing = []
        f <??> (Just xs) = map f xs

        toParent :: String -> ParentEpicNumber
        toParent s = if head s == '#' then SharpEpicNumber s else QuestionEpicNumber s
