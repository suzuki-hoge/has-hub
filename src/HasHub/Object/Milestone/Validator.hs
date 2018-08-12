module HasHub.Object.Milestone.Validator
(
  areAllIn
)
where


import HasHub.Object.Milestone.Type
import HasHub.FixMe2 (Error2, Validation)
import qualified HasHub.FixMe2 as F (areAllIn)


areAllIn :: [MilestoneTitle2] -> [Milestone2] -> Validation [Error2] ()
areAllIn needles haystacks = needles `F.areAllIn` (map toMilestoneTitle haystacks)
  where
    toMilestoneTitle :: Milestone2 -> MilestoneTitle2
    toMilestoneTitle (Milestone2 _ title _ _) = title
