module HasHub.Object.Collaborator.Validator
(
  areAllIn
)
where


import HasHub.Object.Collaborator.Type

import HasHub.FixMe2 (Error2, Validation)
import qualified HasHub.FixMe2 as F (areAllIn)


areAllIn :: [Collaborator2] -> [Collaborator2] -> Validation [Error2] ()
areAllIn = F.areAllIn
