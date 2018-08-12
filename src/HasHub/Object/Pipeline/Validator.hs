module HasHub.Object.Pipeline.Validator
(
  areAllIn
)
where


import HasHub.Object.Pipeline.Type
import HasHub.FixMe2 (Error2, Validation)
import qualified HasHub.FixMe2 as F (areAllIn)


areAllIn :: [PipelineName2] -> [Pipeline2] -> Validation [Error2] ()
areAllIn needles haystacks = needles `F.areAllIn` (map toPipelineName haystacks)
  where
    toPipelineName :: Pipeline2 -> PipelineName2
    toPipelineName (Pipeline2 _ name) = name
