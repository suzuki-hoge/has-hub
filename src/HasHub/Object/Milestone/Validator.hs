{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Milestone.Validator
(
  areAllIn
, module HasHub.FixMe
, module HasHub.Object.Milestone.Type
)
where


import HasHub.Object.Milestone.Type

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn)


instance FixMe (NonExistentError MilestoneTitle) where
  toMessage (NonExistentError (MilestoneTitle title)) = "no such milestone: " ++ title


areAllIn :: [MilestoneTitle] -> [Milestone] -> Validation [NonExistentError MilestoneTitle] ()
areAllIn needles haystacks = needles `F.areAllIn` map _title haystacks
