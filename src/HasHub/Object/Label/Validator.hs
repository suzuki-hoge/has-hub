module HasHub.Object.Label.Validator
(
  areAllIn
, module HasHub.FixMe
)
where


import HasHub.Object.Label.Type

import HasHub.FixMe (Error, Validation(..))
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [Label] -> [Label] -> Validation [Error] ()
areAllIn = F.areAllIn
