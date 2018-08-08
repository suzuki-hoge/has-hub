{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.Data where


import Data.Aeson (FromJSON(..), Value(Object), ToJSON(..), (.:), (.:?), (.=), object)

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybe, listToMaybe)


data MilestoneNumber = MilestoneNumber Int deriving (Eq, Show)
instance FromJSON MilestoneNumber where
  parseJSON (Object v) = MilestoneNumber <$> (v .: "number")


newtype MilestoneTitle = MilestoneTitle String deriving (Eq)
instance Show MilestoneTitle where
  show (MilestoneTitle v) = v


newtype DueOn = DueOn String deriving (Eq)
instance Show DueOn where
  show (DueOn v) = head . (splitOn "T") $ v


data GitHubInput = GitHubInput MilestoneTitle (Maybe DueOn) deriving (Eq, Show)
instance ToJSON GitHubInput where
  toJSON (GitHubInput (MilestoneTitle t) md) = object $ [
    "title" .= t
    ] ++ maybe [] (\(DueOn d) -> ["due_on" .= d]) md


data GitHubOutput = GitHubOutput MilestoneNumber MilestoneTitle (Maybe DueOn) deriving (Eq, Show)
instance FromJSON GitHubOutput where
  parseJSON (Object v) = GitHubOutput <$> (MilestoneNumber <$> v .: "number") <*> (MilestoneTitle <$> v .: "title") <*> (fmap DueOn <$> v .:? "due_on")


newtype StartOn = StartOn String deriving (Eq)
instance ToJSON StartOn where
  toJSON (StartOn s) = object ["start_date" .= s]
instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")
instance Show StartOn where
  show (StartOn v) = head . (splitOn "T") $ v


data Milestone = Milestone MilestoneNumber MilestoneTitle (Maybe StartOn) (Maybe DueOn) deriving (Eq)
instance Show Milestone where
  show (Milestone _ title startOn dueOn) = (show title) ++ " (" ++ (showMaybe startOn) ++ " ~ " ++ (showMaybe dueOn) ++ ")"
    where
      showMaybe :: (Show a) => Maybe a -> String
      showMaybe = (fromMaybe "          ") . (fmap show)


data YamlWrappedMilestone = YamlWrappedMilestone
                  String         -- title
                  (Maybe String) -- start_on
                  (Maybe String) -- due_on
               deriving (Eq, Show)
instance FromJSON YamlWrappedMilestone where
  parseJSON (Object v) = YamlWrappedMilestone <$> (v .: "title") <*> (v .:? "start_on") <*> (v .:? "due_on")


filterBy :: [Milestone] -> Maybe MilestoneTitle -> Maybe Milestone
filterBy ms Nothing = Nothing
filterBy ms (Just mt) = listToMaybe $ filter (\(Milestone _ t _ _) -> t == mt) ms
