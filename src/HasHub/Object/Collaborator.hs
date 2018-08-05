module HasHub.Object.Collaborator
(
  validate
, module HasHub.Object.Collaborator.Data
)
where


import Data.Either.Validation (Validation(..))

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, getGitHub)
import HasHub.Object.Collaborator.Data
import HasHub.Object.FixMe (FixMe, areAllContains)


validate :: C.Client -> [Collaborator] -> IO (Validation [FixMe] [Collaborator])
validate client needles = do
  putStrLn "\nvalidate Collaborators"

  haystacks <- getAll client

  return $ haystacks `areAllContains` needles


getAll :: C.Client -> IO [Collaborator] -- todo spec
getAll client = do
  putStrLn "  fetch all Collaborators"

  json <- C.getGitHub client "/collaborators" []

  return . fromJust $ (decode json :: Maybe [Collaborator])
