{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), decode, ToJSON(..), object, (.=))

import Data.Maybe (fromJust)


data MilestoneNumber2 = MilestoneNumber2 Int deriving (Eq, Show)
instance FromJSON MilestoneNumber2 where
  parseJSON (Object v) = MilestoneNumber2 <$> (v .: "number")


newtype MilestoneTitle2 = MilestoneTitle2 String deriving (Eq, Show)


newtype DueOn2 = DueOn2 String deriving (Eq, Show)


data CreateMilestoneInput = CreateMilestoneInput MilestoneTitle2 (Maybe DueOn2) deriving (Eq, Show)
instance ToJSON CreateMilestoneInput where
  toJSON (CreateMilestoneInput (MilestoneTitle2 t) dueOn) = object $ [
    "title" .= t
    ] ++ maybe [] (\(DueOn2 d) -> ["due_on" .= d]) dueOn


data CreateMilestoneOutput = CreateMilestoneOutput MilestoneNumber2 MilestoneTitle2 (Maybe DueOn2) deriving (Eq, Show)
instance FromJSON CreateMilestoneOutput where
  parseJSON (Object v) = CreateMilestoneOutput <$> (MilestoneNumber2 <$> v .: "number") <*> (MilestoneTitle2 <$> v .: "title") <*> (fmap DueOn2 <$> v .:? "due_on")


decodeJust :: LBS.ByteString -> [CreateMilestoneOutput]
decodeJust = fromJust . decode


newtype StartOn2 = StartOn2 String deriving (Eq, Show)
instance ToJSON StartOn2 where
  toJSON (StartOn2 s) = object ["start_date" .= s]
instance FromJSON StartOn2 where
  parseJSON (Object v) = StartOn2 <$> (v .: "start_date")


data Milestone2 = Milestone2 MilestoneNumber2 MilestoneTitle2 (Maybe StartOn2) (Maybe DueOn2) deriving (Eq, Show)
