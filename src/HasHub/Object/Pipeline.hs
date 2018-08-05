module HasHub.Object.Pipeline
(
  validate
, module HasHub.Object.Pipeline.Data
)
where


import Data.Either.Validation (Validation(..))

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, getZenHub)
import HasHub.Object.Pipeline.Data
import HasHub.Object.FixMe (FixMe, areAllContains)


validate :: C.Client -> [PipelineName] -> IO (Validation [FixMe] [Pipeline])
validate client needles = do
  putStrLn "\nvalidate PipelineNames"

  pipelines <- getAll client
  let names = map (\(Pipeline id name) -> PipelineName name) pipelines
  let vs =  names `areAllContains` needles

  return $ (\_ -> pipelines) <$> vs


getAll :: C.Client -> IO [Pipeline]
getAll client = do
  putStrLn "  fetch all Pipelines"

  json <- C.getZenHub client "/board"

  return . fromJust $ (parseInList json)
