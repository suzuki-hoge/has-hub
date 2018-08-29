module HasHub.Command.CreateMilestones
(
  execute
)
where


import Text.Printf (printf)

import qualified HasHub.Command.ReferAll as RA

import HasHub.Object.Milestone.Parser as Parser

import HasHub.Object.Milestone.Client as MC
import HasHub.Object.Milestone.Validator as MV

import HasHub.FixMe


execute :: [FilePath] -> IO ()
execute fsp = do
  parsed <- Parser.readObjects fsp

  case parsed of
    Success yamls -> do
      putStrLn "\nrefer for yaml data validation."

      milestones <- MC.referAll

      case flat [
          _message $ _titles yamls `MV.areAllNotIn` milestones
        , _message $ MV.dueOnFormat $ _dueOns yamls
        , _message $ MV.startOnFormat $ _startOns yamls
        ] of
        Success ()     -> createAll yamls
        Failure errors -> printMessages errors

    Failure fms -> printFixMes fms


type Current = Int
type Total = Int


createAll :: [YamlMilestone] -> IO ()
createAll yamls = createAll' (length yamls) yamls
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
