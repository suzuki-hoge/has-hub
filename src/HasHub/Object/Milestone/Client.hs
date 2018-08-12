module HasHub.Object.Milestone.Client
(
  referAll
, module HasHub.Object.Milestone.Type
)
where


import Data.Aeson (decode)
import Data.Maybe (fromJust)

import HasHub.Connection.Connector (getGitHub, getZenHub)
import HasHub.Object.Milestone.Type


referAll :: IO [Milestone]
referAll = decodeJust <$> getGitHub "/milestones" >>= mapM withStartOn
  where
    withStartOn :: CreateMilestoneOutput -> IO Milestone
    withStartOn (CreateMilestoneOutput number title dueOn) = do
      let (MilestoneNumber n) = number                                                  -- todo resource interface
      startOn <- decode <$> getZenHub ("/milestones/" ++ (show $ n) ++ "/start_date")
      return $ Milestone number title startOn dueOn
