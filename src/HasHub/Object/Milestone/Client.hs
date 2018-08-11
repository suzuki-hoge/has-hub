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


referAll :: IO [Milestone2]
referAll = do
  json <- getGitHub "/milestones"

  let outputs = fromJust $ (decode json :: Maybe [CreateMilestoneOutput])

  mapM withStartOn outputs

    where
      withStartOn :: CreateMilestoneOutput -> IO Milestone2
      withStartOn (CreateMilestoneOutput number title dueOn) = do
        let (MilestoneNumber2 n) = number                                                  -- todo resource interface
        startOn <- decode <$> getZenHub ("/milestones/" ++ (show $ n) ++ "/start_date")
        return $ Milestone2 number title startOn dueOn
