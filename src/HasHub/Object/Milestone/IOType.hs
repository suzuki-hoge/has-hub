{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), decode, ToJSON(..), object, (.=))

import Data.Maybe (fromJust)

import HasHub.Object.Milestone.Type

import HasHub.Connection.Config.Type (ToResource(..))


-- input


data ReferGitHubInput = ReferGitHubInput

instance ToResource ReferGitHubInput where
  toResource _ = "/milestones"

--
--data CreateMilestoneInput = CreateMilestoneInput MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

--instance ToJSON CreateMilestoneInput where
--  toJSON (CreateMilestoneInput (MilestoneTitle t) dueOn) = object $ [
--    "title" .= t
--    ] ++ maybe [] (\(DueOn d) -> ["due_on" .= d]) dueOn


newtype ReferStartOnInput = ReferStartOnInput MilestoneNumber

instance ToResource ReferStartOnInput where
  toResource (ReferStartOnInput (MilestoneNumber n)) = "/milestones/" ++ show n ++ "/start_date"


instance ToJSON StartOn where
  toJSON (StartOn s) = object ["start_date" .= s]


-- output


instance FromJSON MilestoneNumber where
  parseJSON (Object v) = MilestoneNumber <$> (v .: "number")


data ReferGitHubOutput = ReferGitHubOutput MilestoneNumber MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

instance FromJSON ReferGitHubOutput where
  parseJSON (Object v) = ReferGitHubOutput <$> (MilestoneNumber <$> v .: "number") <*> (MilestoneTitle <$> v .: "title") <*> (fmap DueOn <$> v .:? "due_on")

asGitHubOutputs :: LBS.ByteString -> [ReferGitHubOutput]
asGitHubOutputs = fromJust . decode


instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")

asStartOn :: LBS.ByteString -> Maybe StartOn
asStartOn = decode
