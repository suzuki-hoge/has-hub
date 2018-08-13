module HasHub.Object.Milestone.Validator
(
  areAllIn
, module HasHub.FixMe
)
where


import HasHub.Object.Milestone.Type

import HasHub.FixMe (Error, Validation(..))
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [MilestoneTitle] -> [Milestone] -> Validation [Error] ()
areAllIn needles haystacks = needles `F.areAllIn` map _title haystacks
