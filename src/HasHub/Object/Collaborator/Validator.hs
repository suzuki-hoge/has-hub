{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Collaborator.Validator
(
  areAllIn
, module HasHub.FixMe
, module HasHub.Object.Collaborator.Type
)
where


import HasHub.Object.Collaborator.Type

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn)


instance FixMe (NonExistentError Collaborator) where
  toMessage (NonExistentError (Collaborator name)) = "no such assignee: " ++ name


areAllIn :: [Collaborator] -> [Collaborator] -> Validation [NonExistentError Collaborator] ()
areAllIn = F.areAllIn
