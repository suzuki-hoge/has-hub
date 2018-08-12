module HasHub.Object.Pipeline.Validator
(
  areAllIn
, module HasHub.FixMe
)
where


import HasHub.Object.Pipeline.Type
import HasHub.FixMe (Error, Validation(..))
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [PipelineName] -> [Pipeline] -> Validation [Error] ()
areAllIn needles haystacks = needles `F.areAllIn` (map toPipelineName haystacks)
  where
    toPipelineName :: Pipeline -> PipelineName
    toPipelineName (Pipeline _ name) = name
