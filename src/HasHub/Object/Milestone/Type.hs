module HasHub.Object.Milestone.Type where


import Data.List.Split (splitOn)


import Data.List (find)


newtype MilestoneNumber = MilestoneNumber Int deriving (Eq, Show)


newtype MilestoneTitle = MilestoneTitle String deriving (Eq, Show)


newtype StartOn = StartOn String deriving (Eq, Show)


newtype DueOn = DueOn String deriving (Eq, Show)


data Milestone = Milestone MilestoneNumber MilestoneTitle (Maybe StartOn) (Maybe DueOn) deriving (Eq, Show)


_title :: Milestone -> MilestoneTitle
_title (Milestone _ title _ _) = title


findIn :: [Milestone] -> MilestoneTitle -> Maybe Milestone
findIn milestones milestoneTitle = find (\milestone -> _title milestone == milestoneTitle) milestones


_string :: MilestoneTitle -> Maybe StartOn -> Maybe DueOn -> String
_string (MilestoneTitle t) startOn dueOn = t ++ " (" ++ (_startOnString startOn) ++ " ~ " ++ (_dueOnString dueOn) ++ ")"
  where
    _startOnString :: Maybe StartOn -> String
    _startOnString Nothing            = "          "
    _startOnString (Just (StartOn s)) = head $ splitOn "T" s

    _dueOnString :: Maybe DueOn -> String
    _dueOnString Nothing          = "          "
    _dueOnString (Just (DueOn s)) = head $ splitOn "T" s
