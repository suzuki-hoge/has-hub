module HasHub.Object.Pipeline.Type where


newtype PipelineId = PipelineId String deriving (Eq, Show)


newtype PipelineName = PipelineName String deriving (Eq, Show)


data Pipeline = Pipeline PipelineId PipelineName deriving (Eq, Show)


_name :: Pipeline -> PipelineName
_name (Pipeline _ name) = name
