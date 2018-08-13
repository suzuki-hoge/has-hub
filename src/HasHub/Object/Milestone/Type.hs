{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), decode, ToJSON(..), object, (.=))

import Data.Maybe (fromJust)

import HasHub.Connection.Type (ToResource(..))


data ReferGitHubInput = ReferGitHubInput
instance ToResource ReferGitHubInput where
  toResource _ = "/milestones"


newtype MilestoneNumber = MilestoneNumber Int deriving (Eq, Show)
instance FromJSON MilestoneNumber where
  parseJSON (Object v) = MilestoneNumber <$> (v .: "number")


newtype MilestoneTitle = MilestoneTitle String deriving (Eq, Show)


newtype DueOn = DueOn String deriving (Eq, Show)

--
--data CreateMilestoneInput = CreateMilestoneInput MilestoneTitle (Maybe DueOn) deriving (Eq, Show)
--instance ToJSON CreateMilestoneInput where
--  toJSON (CreateMilestoneInput (MilestoneTitle t) dueOn) = object $ [
--    "title" .= t
--    ] ++ maybe [] (\(DueOn d) -> ["due_on" .= d]) dueOn


data ReferGitHubOutput = ReferGitHubOutput MilestoneNumber MilestoneTitle (Maybe DueOn) deriving (Eq, Show)
instance FromJSON ReferGitHubOutput where
  parseJSON (Object v) = ReferGitHubOutput <$> (MilestoneNumber <$> v .: "number") <*> (MilestoneTitle <$> v .: "title") <*> (fmap DueOn <$> v .:? "due_on")


decodeJust :: LBS.ByteString -> [ReferGitHubOutput]
decodeJust = fromJust . decode


data ReferStartOnInput = ReferStartOnInput MilestoneNumber
instance ToResource ReferStartOnInput where
  toResource (ReferStartOnInput (MilestoneNumber n)) = "/milestones/" ++ show n ++ "/start_date"


newtype StartOn = StartOn String deriving (Eq, Show)
instance ToJSON StartOn where
  toJSON (StartOn s) = object ["start_date" .= s]
instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")


decodeJust'' :: LBS.ByteString -> Maybe StartOn -- todo name ?
decodeJust'' = decode


data Milestone = Milestone MilestoneNumber MilestoneTitle (Maybe StartOn) (Maybe DueOn) deriving (Eq, Show)


_title :: Milestone -> MilestoneTitle
_title (Milestone _ title _ _) = title
