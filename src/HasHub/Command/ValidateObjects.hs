module HasHub.Command.ValidateObjects
(
  execute
, execute'
)
where


import HasHub.Object.Object.Parser as Parser

import qualified HasHub.Command.ReferAll as RA

import HasHub.Object.Object.Validator as OV
import HasHub.Object.Pipeline.Validator as PV
import HasHub.Object.Label.Validator as LV
import HasHub.Object.Collaborator.Validator as CV
import HasHub.Object.Milestone.Validator as MV

import HasHub.FixMe (flat, _message, printMessages, printFixMes, Validation(..))


execute :: [FilePath] -> IO ()
execute fps = do
  putStrLn "\nvalidate all objects."

  parsed <- Parser.readObjects fps

  case parsed of
    Success yamls -> do
      validated <- execute' yamls

      case validated of
        Failure errors -> printMessages errors
        Success _      -> return ()

    Failure fms -> printFixMes fms

  putStrLn "\nall validation succeeded."


execute' :: [YamlObject] -> IO (Validation [String] ([Pipeline], [Milestone], [Epic]))
execute' yamls = do
  putStrLn "\nrefer all for yaml data validation."

  (epics, pipelines, labels, collaborators, milestones) <- RA.execute'

  case flat [
      _message $ _linkingEpicNumbers yamls `OV.areAllIn` epics
    , _message $ _pipelineNames yamls `PV.areAllIn` pipelines
    , _message $ _labels yamls `LV.areAllIn` labels
    , _message $ _collaborators yamls `CV.areAllIn` collaborators
    , _message $ _milestoneTitles yamls `MV.areAllIn` milestones
    , _message $ OV.noDuplication $ _epicLinkNumbersWithDuplication yamls
    , _message $ OV.linkNumberFormat $ _epicLinkNumbers yamls
    , _message $ OV.linkingNumberFormat $ _linkingEpicNumbers yamls
    , _message $ _linkings yamls `OV.linkTo` _linkeds yamls
    ] of
    Success ()     -> return $ Success (pipelines, milestones, epics)
    Failure errors -> return $ Failure errors
