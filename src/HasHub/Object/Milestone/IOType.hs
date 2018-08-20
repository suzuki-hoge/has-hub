{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), decode, ToJSON(..), object, (.=))

import HasHub.Object.Milestone.Type

import HasHub.Connection.Config.Type (ToResource(..))

import HasHub.FixMe (asJust)


-- input


data ReferGitHubInput = ReferGitHubInput

instance ToResource ReferGitHubInput where
  toResource _ = "/milestones"


data CreateMilestoneInput = CreateMilestoneInput MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

instance ToResource CreateMilestoneInput where
  toResource _ = "/milestones"

instance ToJSON CreateMilestoneInput where
  toJSON (CreateMilestoneInput (MilestoneTitle t) dueOn) = object $ [
    "title" .= t
    ] ++ maybe [] (\(DueOn d) -> ["due_on" .= d]) dueOn


newtype ReferStartOnInput = ReferStartOnInput MilestoneNumber

instance ToResource ReferStartOnInput where
  toResource (ReferStartOnInput (MilestoneNumber n)) = "/milestones/" ++ show n ++ "/start_date"


data CreateStartOnInput = CreateStartOnInput MilestoneNumber StartOn

instance ToResource CreateStartOnInput where
  toResource (CreateStartOnInput (MilestoneNumber n) _) = "/milestones/" ++ show n ++ "/start_date"

instance ToJSON CreateStartOnInput where
  toJSON (CreateStartOnInput _ (StartOn s)) = object ["start_date" .= s]


-- output


instance FromJSON MilestoneNumber where
  parseJSON (Object v) = MilestoneNumber <$> (v .: "number")


asNumber :: LBS.ByteString -> IO MilestoneNumber
asNumber lbs = asJust $ decode lbs


data ReferGitHubOutput = ReferGitHubOutput MilestoneNumber MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

instance FromJSON ReferGitHubOutput where
  parseJSON (Object v) = ReferGitHubOutput <$> (MilestoneNumber <$> v .: "number") <*> (MilestoneTitle <$> v .: "title") <*> (fmap DueOn <$> v .:? "due_on")

asGitHubOutputs :: LBS.ByteString -> IO [ReferGitHubOutput]
asGitHubOutputs lbs = asJust $ decode lbs


instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")


asStartOn :: LBS.ByteString -> Maybe StartOn
asStartOn = decode
