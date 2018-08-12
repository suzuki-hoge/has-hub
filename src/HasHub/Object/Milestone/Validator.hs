module HasHub.Object.Milestone.Validator
(
  areAllIn
)
where


import HasHub.Object.Milestone.Type
import HasHub.FixMe (Error, Validation)
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [MilestoneTitle] -> [Milestone] -> Validation [Error] ()
areAllIn needles haystacks = needles `F.areAllIn` (map toMilestoneTitle haystacks)
  where
    toMilestoneTitle :: Milestone -> MilestoneTitle
    toMilestoneTitle (Milestone _ title _ _) = title
