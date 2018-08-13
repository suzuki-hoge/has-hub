module HasHub.Object.Label.Client
(
  referAll
, module HasHub.Object.Label.Type
)
where


import HasHub.Object.Label.Type

import HasHub.Connection.Connector (getGitHub)


referAll :: IO [Label]
referAll = decodeJust <$> getGitHub "/labels"
