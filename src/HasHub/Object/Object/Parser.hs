{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Parser where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import HasHub.Yaml.Reader (readYaml)

import HasHub.Object.Object.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Pipeline.Type
import HasHub.Connection.Type (RepositoryId)

import HasHub.FixMe2 (Validation(..), Error2)


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
  parseJSON (Object v) = YamlWrappedObject2 <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "pipeline") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "milestone") <*> (v .:? "estimate") <*> (v .:? "epics")


read :: FilePath -> IO (Validation [Error2] [YamlObject2])
read = readYaml mapping
  where
    mapping :: YamlWrappedObject2 -> YamlObject2
    mapping (YamlWrappedObject2 meln t mb mpn ls cs mmt me es) = createEither meln (Title2 t) (Body2 <?> mb) (PipelineName2 <$> mpn) (Label2 <??> ls) (Collaborator2 <??> cs) (MilestoneTitle2 <$> mmt) (Estimate2 <$> me) (toParent <??> es)
      where
        createEither (Just eln) = EpicYamlObject2 (EpicLinkNumber2 eln)
        createEither Nothing    = IssueYamlObject2

        (<?>) :: (String -> a) -> Maybe String -> a
        f <?> Nothing = f ""
        f <?> (Just xs) = f xs


        (<??>) :: (a -> b) -> Maybe [a] -> [b]
        f <??> Nothing = []
        f <??> (Just xs) = map f xs

        toParent :: String -> ParentEpicNumber2
        toParent s = if head s == '#' then SharpEpicNumber2 s else QuestionEpicNumber2 s
