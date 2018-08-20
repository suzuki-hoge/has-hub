{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.Parser where


import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import Data.List (nub)
import Data.Maybe (mapMaybe)

import HasHub.Yaml.Reader (readYaml, YamlReadingError(..))

import HasHub.Object.Milestone.Type

import HasHub.FixMe (Validation(..))


data YamlMilestone = YamlMilestone MilestoneTitle (Maybe StartOn) (Maybe DueOn) deriving (Eq, Show)

instance FromJSON YamlMilestone where
  parseJSON (Object v) = YamlMilestone <$> (MilestoneTitle <$> v .: "title") <*> (fmap _startOn <$> v .:? "start_on") <*> (fmap _dueOn <$> v .:? "due_on")
    where
      _startOn s = StartOn $ s ++ "T00:00:00Z"
      _dueOn s = DueOn $ s ++ "T23:59:59Z"


readObjects :: FilePath -> IO (Validation [YamlReadingError] [YamlMilestone])
readObjects = readYaml mapping
  where
    mapping :: YamlMilestone -> YamlMilestone
    mapping = id


_titles :: [YamlMilestone] -> [MilestoneTitle]
_titles = map _title
  where
    _title :: YamlMilestone -> MilestoneTitle
    _title (YamlMilestone x _ _) = x


_startOns :: [YamlMilestone] -> [StartOn]
_startOns = nub . mapMaybe _startOn
  where
    _startOn :: YamlMilestone -> Maybe StartOn
    _startOn (YamlMilestone _ x _) = x


_dueOns :: [YamlMilestone] -> [DueOn]
_dueOns = nub . mapMaybe _dueOn
  where
    _dueOn :: YamlMilestone -> Maybe DueOn
    _dueOn (YamlMilestone _ _ x) = x
