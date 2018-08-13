module HasHub.Object.Collaborator.Client
(
  referAll
, module HasHub.Object.Collaborator.Type
)
where


import HasHub.Object.Collaborator.IOType
import HasHub.Object.Collaborator.Type

import HasHub.Connection.Connector (getGitHub)


referAll :: IO [Collaborator]
referAll = asCollaborators <$> getGitHub ReferInput
