module HasHub.Object.Milestone.Client
(
  referAll
, create
, module HasHub.Object.Milestone.Type
)
where


import Text.Printf (printf)

import HasHub.Object.Milestone.IOType
import HasHub.Object.Milestone.Type

import HasHub.Connection.Connector (getGitHub, getZenHub, postGitHub, postZenHub_)


referAll :: IO [Milestone]
referAll = do
  putStrLn "  refer all Milestones"

  getGitHub ReferGitHubInput >>= asGitHubOutputs >>= mapM withStartOn
  where
    withStartOn :: ReferGitHubOutput -> IO Milestone
    withStartOn (ReferGitHubOutput number title dueOn) = do
      startOn <- asStartOn <$> getZenHub (ReferStartOnInput number)
      return $ Milestone number title startOn dueOn


create :: MilestoneTitle -> Maybe StartOn -> Maybe DueOn -> IO ()
create title startOn dueOn = do
  printf "  create %s\n" (_string title startOn dueOn)

  number <- postGitHub (CreateMilestoneInput title dueOn) >>= asNumber

  mapM_ (setStartOn number) startOn


setStartOn :: MilestoneNumber -> StartOn -> IO ()
setStartOn number startOn = postZenHub_ $ CreateStartOnInput number startOn
