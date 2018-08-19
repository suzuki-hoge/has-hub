module HasHub.Command.CreateMilestones
(
  execute
)
where


import Text.Printf (printf)

import qualified HasHub.Command.ReferAll as RA

import HasHub.Object.Milestone.Parser as Parser

import HasHub.Object.Milestone.Client as MC

import HasHub.FixMe


execute :: FilePath -> IO ()
execute yaml = do
  parsed <- Parser.readObjects yaml

  case parsed of
    Success(yamls) -> createAll yamls
    Failure fms -> printFixMes fms


type Current = Int
type Total = Int


createAll :: [YamlMilestone] -> IO ()
createAll yamls = createAll' (length yamls) yamls -- todo objects -> yamls
  where
    createAll' :: Int -> [YamlMilestone] -> IO ()
    createAll' total []           = printf "\n%d milestones created.\n" total
    createAll' total (yaml:yamls) = do
      create yaml (total - length yamls) total

      createAll' total yamls
      where
        create :: YamlMilestone -> Current -> Total -> IO ()
        create (YamlMilestone title startOn dueOn) current total = do
          printf "\ncreate Milestone. (%d / %d)\n" current total

          MC.create title startOn dueOn


-- todo yaml format validation
-- todo check existence