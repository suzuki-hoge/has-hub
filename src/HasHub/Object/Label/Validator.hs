{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Label.Validator
(
  areAllIn
, module HasHub.FixMe
, module HasHub.Object.Label.Type
)
where


import HasHub.Object.Label.Type

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn)


instance FixMe (NonExistentError Label) where
  toMessage (NonExistentError (Label name)) = "no such label: " ++ name


areAllIn :: [Label] -> [Label] -> Validation [NonExistentError Label] ()
areAllIn = F.areAllIn
