module HasHub.Object.Collaborator.Validator
(
  areAllIn
)
where


import HasHub.Object.Collaborator.Type

import HasHub.FixMe (Error, Validation)
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [Collaborator] -> [Collaborator] -> Validation [Error] ()
areAllIn = F.areAllIn
