module HasHub.Object.Label.Client
(
  referAll
, module HasHub.Object.Label.Type
)
where


import HasHub.Object.Label.IOType
import HasHub.Object.Label.Type

import HasHub.Connection.Connector (getGitHub)


referAll :: IO [Label]
referAll = do
  putStrLn "  refer all Labels"

  getGitHub ReferInput >>= asLabels
