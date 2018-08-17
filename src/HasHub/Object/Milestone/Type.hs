module HasHub.Object.Milestone.Type where


import Data.List (find)


newtype MilestoneNumber = MilestoneNumber Int deriving (Eq, Show)


newtype MilestoneTitle = MilestoneTitle String deriving (Eq, Show)


newtype DueOn = DueOn String deriving (Eq, Show)


newtype StartOn = StartOn String deriving (Eq, Show)


data Milestone = Milestone MilestoneNumber MilestoneTitle (Maybe StartOn) (Maybe DueOn) deriving (Eq, Show)


_title :: Milestone -> MilestoneTitle
_title (Milestone _ title _ _) = title


findIn :: [Milestone] -> MilestoneTitle -> Maybe Milestone
findIn milestones milestoneTitle = find (\milestone -> _title milestone == milestoneTitle) milestones
