module HasHub.Object.Collaborator.Client
(
  referAll
, module HasHub.Object.Collaborator.Type
)
where


import Data.Aeson (decode)
import Data.Maybe (fromJust)

import HasHub.Connection.Connector (getGitHub)
import HasHub.Object.Collaborator.Type


referAll :: IO [Collaborator2]
referAll = decodeJust <$> getGitHub "/collaborators"
