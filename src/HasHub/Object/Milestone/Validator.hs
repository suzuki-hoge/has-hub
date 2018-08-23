{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Milestone.Validator
(
  areAllIn
, areAllNotIn
, dueOnFormat
, startOnFormat
, ExistingError(..)
, module HasHub.FixMe
, module HasHub.Object.Milestone.Type
)
where


import Text.Regex.Posix ((=~))
import Data.List.Split (splitOn)

import HasHub.Object.Milestone.Type

import HasHub.FixMe (Validation(..), FixMe(..), FormatError(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn, (??))


instance FixMe (NonExistentError MilestoneTitle) where
  toMessage (NonExistentError (MilestoneTitle title)) = "no such milestone: " ++ title


areAllIn :: [MilestoneTitle] -> [Milestone] -> Validation [NonExistentError MilestoneTitle] ()
areAllIn needles haystacks = needles `F.areAllIn` map _title haystacks


newtype ExistingError = ExistingError MilestoneTitle deriving (Eq, Show)

instance FixMe ExistingError where
  toMessage (ExistingError (MilestoneTitle t)) = "already existing milestone: " ++ t


areAllNotIn :: [MilestoneTitle] -> [Milestone] -> Validation [ExistingError] ()
areAllNotIn needles haystacks = map (notContains (map _title haystacks)) needles F.?? ()
  where
    notContains :: [MilestoneTitle] -> MilestoneTitle -> Maybe ExistingError
    notContains haystacks needle = if needle `notElem` haystacks
      then Nothing
      else Just $ ExistingError needle


instance FixMe (FormatError DueOn) where
  toMessage (FormatError (DueOn s))   = "not satisfied ^yyyy-mm-dd$ format: " ++ head (splitOn "T" s)

instance FixMe (FormatError StartOn) where
  toMessage (FormatError (StartOn s)) = "not satisfied ^yyyy-mm-dd$ format: " ++ head (splitOn "T" s)


dueOnFormat :: [DueOn] -> Validation [FormatError DueOn] ()
dueOnFormat dueOns = map validate dueOns F.?? ()
  where
    validate :: DueOn -> Maybe (FormatError DueOn)
    validate dueOn@(DueOn s) = isDate s dueOn


startOnFormat :: [StartOn] -> Validation [FormatError StartOn] ()
startOnFormat startOns = map validate startOns F.?? ()
  where
    validate :: StartOn -> Maybe (FormatError StartOn)
    validate startOn@(StartOn s) = isDate s startOn


isDate :: String -> a -> Maybe (FormatError a)
isDate s date = if s =~ "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  then Nothing
  else Just $ FormatError date
