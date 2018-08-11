module HasHub.Object.Pipeline.Client
(
  referAll
, module HasHub.Object.Pipeline.Type
)
where


import HasHub.Connection.Connector (getZenHub)
import HasHub.Object.Pipeline.Type


referAll :: IO [Pipeline2]
referAll = decodeJust <$> getZenHub "/board"

