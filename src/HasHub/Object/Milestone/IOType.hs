{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Milestone.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), decode, ToJSON(..), object, (.=))
import Data.Aeson.Types (parseMaybe)

import HasHub.Object.Milestone.Type

import HasHub.Connection.Config.Type (ToResource(..), QueryParser(..), PaginationQueryParser(..), mkAfter)

import HasHub.FixMe (asJust)


-- refer git-hub-milestones


data ReferGitHubMilestonesInput = ReferGitHubMilestonesInput

instance QueryParser ReferGitHubMilestonesInput where
  toQueryPart _ owner repository cursor = unlines [
      "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    milestones(first:100, states:OPEN" ++ mkAfter cursor ++ ") {"
    , "      nodes {"
    , "        number"
    , "        title"
    , "        dueOn"
    , "      }"
    , "      pageInfo {"
    , "        hasNextPage"
    , "        endCursor"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]

instance PaginationQueryParser ReferGitHubMilestonesInput where
  parseHasNext _ = asJust . parse
    where
      parse :: LBS.ByteString -> Maybe Bool
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "milestones")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "hasNextPage")
  parseEndCursor _ = parse
    where
      parse :: LBS.ByteString -> Maybe String
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "milestones")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "endCursor")


data ReferGitHubMilestonesOutput = ReferGitHubMilestonesOutput MilestoneNumber MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

instance FromJSON ReferGitHubMilestonesOutput where
  parseJSON (Object v) = ReferGitHubMilestonesOutput <$> (MilestoneNumber <$> v .: "number") <*> (MilestoneTitle <$> v .: "title") <*> (fmap DueOn <$> v .:? "dueOn")


asGitHubMilestones :: LBS.ByteString -> IO [ReferGitHubMilestonesOutput]
asGitHubMilestones lbs = asJust $ parse lbs
  where
    parse :: LBS.ByteString -> Maybe [ReferGitHubMilestonesOutput]
    parse json = decode json
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: "milestones")
        >>= parseMaybe (.: "nodes")


-- refer start-on


newtype ReferStartOnInput = ReferStartOnInput MilestoneNumber

instance ToResource ReferStartOnInput where
  toResource (ReferStartOnInput (MilestoneNumber n)) = "/milestones/" ++ show n ++ "/start_date"


instance FromJSON StartOn where
  parseJSON (Object v) = StartOn <$> (v .: "start_date")


asStartOn :: LBS.ByteString -> Maybe StartOn
asStartOn = decode


-- create git-hub-milestone


data CreateGitHubMilestoneInput = CreateGitHubMilestoneInput MilestoneTitle (Maybe DueOn) deriving (Eq, Show)

instance ToResource CreateGitHubMilestoneInput where
  toResource _ = "/milestones"

instance ToJSON CreateGitHubMilestoneInput where
  toJSON (CreateGitHubMilestoneInput (MilestoneTitle t) dueOn) = object $ (
    "title" .= t
    ) : maybe [] (\(DueOn d) -> ["due_on" .= d]) dueOn


instance FromJSON MilestoneNumber where
  parseJSON (Object v) = MilestoneNumber <$> (v .: "number")


asNumber :: LBS.ByteString -> IO MilestoneNumber
asNumber lbs = asJust $ decode lbs


-- create start-on


data CreateStartOnInput = CreateStartOnInput MilestoneNumber StartOn

instance ToResource CreateStartOnInput where
  toResource (CreateStartOnInput (MilestoneNumber n) _) = "/milestones/" ++ show n ++ "/start_date"

instance ToJSON CreateStartOnInput where
  toJSON (CreateStartOnInput _ (StartOn s)) = object ["start_date" .= s]
