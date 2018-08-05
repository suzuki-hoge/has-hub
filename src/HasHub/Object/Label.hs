module HasHub.Object.Label
(
  validate
, module HasHub.Object.Label.Data
)
where


import Data.Either.Validation (Validation(..))

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, getGitHub)
import HasHub.Object.Label.Data
import HasHub.Object.FixMe (FixMe, areAllContains)


validate :: C.Client -> [Label] -> IO (Validation [FixMe] [Label])
validate client needles = do
  putStrLn "\nvalidate Labels"

  haystacks <- getAll client

  return $ haystacks `areAllContains` needles


getAll :: C.Client -> IO [Label] -- todo spec
getAll client = do
  putStrLn "  fetch all Labels"

  json <- C.getGitHub client "/labels" []

  return . fromJust $ (decode json :: Maybe [Label])
