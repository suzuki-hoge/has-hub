{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Pipeline.Validator
(
  areAllIn
, module HasHub.FixMe
)
where


import HasHub.Object.Pipeline.Type

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn)


instance FixMe (NonExistentError PipelineName) where
  toMessage (NonExistentError (PipelineName name)) = "no such pipeline: " ++ name


areAllIn :: [PipelineName] -> [Pipeline] -> Validation [NonExistentError PipelineName] ()
areAllIn needles haystacks = needles `F.areAllIn` map _name haystacks
