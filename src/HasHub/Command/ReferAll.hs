module HasHub.Command.ReferAll
(
  execute
, execute'
)
where


import Text.Printf (printf)
import Data.List (sort)

import HasHub.Object.Object.Client as OC
import HasHub.Object.Pipeline.Client as PC
import HasHub.Object.Label.Client as LC
import HasHub.Object.Collaborator.Client as CC
import HasHub.Object.Milestone.Client as MC


execute :: IO ()
execute = do
  putStrLn "\nrefer all."

  (epics, pipelines, labels, collaborators, milestones) <- execute'

  printEpics epics
  printPipelines pipelines
  printLabels labels
  printCollaborators collaborators
  printMilestones milestones

  putStrLn "\nall objects referred."


execute' :: IO ([Epic], [Pipeline], [Label], [Collaborator], [Milestone])
execute' = do
  epics         <- OC.referAll
  pipelines     <- PC.referAll
  labels        <- LC.referAll
  collaborators <- CC.referAll
  milestones    <- MC.referAll

  return (epics, pipelines, labels, collaborators, milestones)


printEpics :: [Epic] -> IO ()
printEpics xs = do
  putStrLn "\nEpics."
  if null xs then putStrLn "  no episc" else mapM_ print (sort xs)
  printResults xs
  where
    print (Epic (EpicNumber n) (Title t)) = printf "  #%d %s\n" n t


printPipelines :: [Pipeline] -> IO ()
printPipelines xs = do
  putStrLn "\nPipelines."
  if null xs then putStrLn "  no pipelines" else mapM_ print xs
  printResults xs
  where
    print (Pipeline _ (PipelineName n)) = printf "  %s\n" n


printLabels :: [Label] -> IO ()
printLabels xs = do
  putStrLn "\nLabels."
  if null xs then putStrLn "  no labels" else mapM_ print xs
  printResults xs
  where
    print (Label n) = printf "  %s\n" n


printCollaborators :: [Collaborator] -> IO ()
printCollaborators xs = do
  putStrLn "\nAssignees."
  if null xs then putStrLn "  no assignees" else mapM_ print xs
  printResults xs
  where
    print (Collaborator n) = printf "  %s\n" n


printMilestones :: [Milestone] -> IO ()
printMilestones xs = do
  putStrLn "\nMilestones."
  if null xs then putStrLn "  no milestones" else mapM_ print (sort xs)
  printResults xs
  where
    print (Milestone _ title startOn dueOn) = printf "  %s\n" (_string title startOn dueOn)


printResults :: [a] -> IO ()
printResults [x] = putStrLn "\n  1 result."
printResults xs  = printf "\n  %d results.\n" $ length xs