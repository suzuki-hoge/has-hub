module HasHub.Object.Label.Validator
(
  areAllIn
)
where


import HasHub.Object.Label.Type

import HasHub.FixMe2 (Error2, Validation)
import qualified HasHub.FixMe2 as F (areAllIn)


areAllIn :: [Label2] -> [Label2] -> Validation [Error2] ()
areAllIn = F.areAllIn
