module HasHub.Object.Milestone.Client
(
  referAll
, module HasHub.Object.Milestone.Type
)
where


import HasHub.Object.Milestone.Type

import HasHub.Connection.Connector (getGitHub, getZenHub)


referAll :: IO [Milestone]
referAll = decodeJust <$> getGitHub ReferGitHubInput >>= mapM withStartOn
  where
    withStartOn :: ReferGitHubOutput -> IO Milestone
    withStartOn (ReferGitHubOutput number title dueOn) = do
      startOn <- decodeJust'' <$> getZenHub (ReferStartOnInput number)
      return $ Milestone number title startOn dueOn
