module HasHub.Object.Milestone.Client
(
  referAll
, module HasHub.Object.Milestone.Type
)
where


import HasHub.Object.Milestone.Type

import HasHub.Connection.Connector (getGitHub, getZenHub)


referAll :: IO [Milestone]
referAll = decodeJust <$> getGitHub "/milestones" >>= mapM withStartOn
  where
    withStartOn :: ReferMilestoneOutput -> IO Milestone
    withStartOn (ReferMilestoneOutput number title dueOn) = do
      let (MilestoneNumber n) = number                                                  -- todo resource interface
      startOn <- decodeJust'' <$> getZenHub ("/milestones/" ++ show n ++ "/start_date")
      return $ Milestone number title startOn dueOn
