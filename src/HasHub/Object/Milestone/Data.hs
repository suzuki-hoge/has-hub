{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.Data where


import Data.Aeson (FromJSON(..), Value(Object), ToJSON(..), (.:), (.:?), (.=), object)

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybe)


data Number = Number Int deriving (Eq, Show)
instance FromJSON Number where
  parseJSON (Object v) = Number <$> (v .: "number")


newtype Title = Title String deriving (Eq)
instance Show Title where
  show (Title v) = v


newtype DueOn = DueOn String deriving (Eq)
instance Show DueOn where
  show (DueOn v) = head . (splitOn "T") $ v


data GitHubInput = GitHubInput Title (Maybe DueOn) deriving (Eq, Show)
instance ToJSON GitHubInput where
  toJSON (GitHubInput (Title t) md) = object $ [
    "title" .= t
    ] ++ maybe [] (\(DueOn d) -> ["due_on" .= d]) md


data GitHubOutput = GitHubOutput Number Title (Maybe DueOn) deriving (Eq, Show)
instance FromJSON GitHubOutput where
  parseJSON (Object v) = GitHubOutput <$> (Number <$> v .: "number") <*> (Title <$> v .: "title") <*> (fmap DueOn <$> v .:? "due_on")


newtype StartOn = StartOn String deriving (Eq)
instance ToJSON StartOn where
  toJSON (StartOn s) = object ["start_date" .= s]
instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")
instance Show StartOn where
  show (StartOn v) = head . (splitOn "T") $ v


data Milestone = Milestone Number Title (Maybe StartOn) (Maybe DueOn) deriving (Eq)
instance Show Milestone where
  show (Milestone _ title startOn dueOn) = (show title) ++ " (" ++ (showMaybe startOn) ++ " ~ " ++ (showMaybe dueOn) ++ ")"
    where
      showMaybe :: (Show a) => Maybe a -> String
      showMaybe = (fromMaybe "          ") . (fmap show)
