module HasHub.Object.Issue
(
  create
)
where


import Text.Printf (printf)
import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, postGitHub, postZenHub, putZenHub, Resource)
import qualified HasHub.Client.Data as Cl

import HasHub.Object.Issue.Data

import qualified HasHub.Object.Milestone.Data as M
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import qualified HasHub.Object.Epic.Data as E


create :: C.Client -> Title -> Body -> Maybe M.Number -> [Collaborator] -> [Label] -> [E.Epic] -> Maybe Pipeline -> Maybe Estimate -> IO Number
create client title body mmn cs ls es mp me = do
  number <- createIssue client title body mmn cs ls

  mapM_ (setParentEpic client number) es
  mapM_ (setPipeline client number) mp
  mapM_ (setEstimate client number) me

  return number


createIssue :: C.Client -> Title -> Body -> Maybe M.Number -> [Collaborator] -> [Label] -> IO Number
createIssue client title body mmn cs ls = do
  printf "create issue(%s) -> " (show title)

  json <- C.postGitHub client "/issues" $ GitHubInput title body mmn cs ls
  let number = fromJust $ (decode json :: Maybe Number)

  printf "%s\n" (show number)

  return number


setParentEpic :: C.Client -> Number -> E.Epic -> IO ()
setParentEpic client number e = do
  printf "[%s] set parent epic(%s)\n" (show number) (show e)

  json <- C.postZenHub client (toResource e) $ EpicInput number (Cl.asRepositoryId client)

  return ()

    where
      toResource :: E.Epic -> C.Resource
      toResource (E.Epic (E.Number n) _) = "/epics/" ++ (show $ n) ++ "/update_issues"


setPipeline :: C.Client -> Number -> Pipeline -> IO ()
setPipeline client number (Pipeline pid pname) = do
  printf "[%s] move to pipeline(%s)\n" (show number) (show pname)

  json <- C.postZenHub client (toResource number) $ PipelineInput pid

  return ()

    where
      toResource :: Number -> C.Resource
      toResource (Number n) = "/issues/" ++ (show $ n) ++ "/moves"


setEstimate :: C.Client -> Number -> Estimate -> IO ()
setEstimate client number e = do
  printf "[%s] set estimate(%s)\n" (show number) (show e)

  json <- C.putZenHub client (toResource number) $ EstimateInput e

  return ()

    where
      toResource :: Number -> C.Resource
      toResource (Number n) = "/issues/" ++ (show $ n) ++ "/estimate"
