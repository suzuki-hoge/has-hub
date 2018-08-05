module HasHub.Object.Milestone
(
  validate
, module HasHub.Object.Milestone.Data
)
where


import Data.Either.Validation (Validation(..))

import Text.Printf (printf)
import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, getGitHub, getZenHub, postGitHub, postZenHub, Resource)
import HasHub.Object.Milestone.Data
import HasHub.Object.FixMe (FixMe, areAllContains)


validate :: C.Client -> [Title] -> IO (Validation [FixMe] [Milestone])
validate client needles = do
  putStrLn "\nvalidate Milestones"

  milestones <- getAll client
  let titles = map (\(Milestone _ title _ _) -> title) milestones
  let vs =  titles `areAllContains` needles

  return $ (\_ -> milestones) <$> vs


getAll :: C.Client -> IO [Milestone]
getAll client = do
  putStrLn "  fetch all Milestones"

  json <- C.getGitHub client "/milestones" []

  let gOuts = fromJust $ (decode json :: Maybe [GitHubOutput])

  mapM (constructWithStartOn client) gOuts

    where
      constructWithStartOn :: C.Client -> GitHubOutput -> IO Milestone
      constructWithStartOn client (GitHubOutput number title dueOn) = do
        printf "  fetch StartOn on Milestone(%s)\n" (show title)

        startOn <- decode <$> C.getZenHub client (milestoneResource number)
        return $ Milestone number title startOn dueOn


create :: C.Client -> Title -> Maybe StartOn -> Maybe DueOn -> IO Milestone
create client title startOn dueOn = do
  printf "\n  create Milestone(%s)\n" (show title)

  json <- C.postGitHub client "/milestones" $ GitHubInput title dueOn

  let number = fromJust $ (decode json :: Maybe Number)

  mapM_ (setStartOn client number) startOn

  return $ Milestone number title startOn dueOn

    where
      setStartOn :: C.Client -> Number -> StartOn -> IO ()
      setStartOn client number startOn = do
        printf "  set StartOn(%s)\n" (show startOn)

        C.postZenHub client (milestoneResource number) startOn

        return ()


milestoneResource :: Number -> C.Resource
milestoneResource (Number n) = "/milestones/" ++ (show $ n) ++ "/start_date"
