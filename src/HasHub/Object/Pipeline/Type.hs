module HasHub.Object.Pipeline.Type where


import Data.List (find)

newtype PipelineId = PipelineId String deriving (Eq, Show)


newtype PipelineName = PipelineName String deriving (Eq, Show)


data Pipeline = Pipeline PipelineId PipelineName deriving (Eq, Show)


_name :: Pipeline -> PipelineName
_name (Pipeline _ name) = name


findIn :: [Pipeline] -> PipelineName -> Maybe Pipeline
findIn pipelines pipelineName = find (\pipeline -> _name pipeline == pipelineName) pipelines
