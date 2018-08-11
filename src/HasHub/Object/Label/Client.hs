module HasHub.Object.Label.Client
(
  referAll
, module HasHub.Object.Label.Type
)
where


import Data.Aeson (decode)
import Data.Maybe (fromJust)

import HasHub.Connection.Connector (getGitHub)
import HasHub.Object.Label.Type


referAll :: IO [Label2]
referAll = do
  json <- getGitHub "/labels"

  return $ fromJust $ (decode json :: Maybe [Label2])
