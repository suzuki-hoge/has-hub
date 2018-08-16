module HasHub.Command.CreateObjects where


import HasHub.Object.Object.Parser as Parser

import HasHub.Object.Object.Client as OC
import HasHub.Object.Pipeline.Client as PC
import HasHub.Object.Label.Client as LC
import HasHub.Object.Collaborator.Client as CC
import HasHub.Object.Milestone.Client as MC

import qualified HasHub.Object.Object.Validator as OV
import qualified HasHub.Object.Pipeline.Validator as PV
import qualified HasHub.Object.Label.Validator as LV
import qualified HasHub.Object.Collaborator.Validator as CV
import qualified HasHub.Object.Milestone.Validator as MV

import HasHub.FixMe (flat, _message, FixMe(..), Validation(..))


createAll :: [YamlObject] -> IO ()
createAll objects = do -- todo next: linked and pick up number by title
  print objects


printErrors :: [String] -> IO ()
printErrors errors = do
  putStrLn "please fix following errors.\n"
  mapM_ putStrLn errors
  putStrLn "\nhas-hub is aborted.\n"


execute :: IO ()
execute = do
--  parsed <- Parser.readObjects "sample/objects/validation_errors.yaml"
  parsed <- Parser.readObjects "sample/objects/epic_and_issue.yaml"
  case parsed of
    Success(objects) -> do
      epicNumbers   <- OC.referAll
      pipelines     <- PC.referAll
      labels        <- LC.referAll
      collaborators <- CC.referAll
      milestones    <- MC.referAll

      case flat [
          _message $ _parentEpicNumbers objects `OV.areAllIn` epicNumbers
        , _message $ _pipelineNames objects `PV.areAllIn` pipelines
        , _message $ _labels objects `LV.areAllIn` labels
        , _message $ _collaborators objects `CV.areAllIn` collaborators
        , _message $ _milestoneTitles objects `MV.areAllIn` milestones
        , _message $ OV.noDuplication $ _epicLinkNumbersWithDuplication objects
        , _message $ OV.linkNumberFormat $ _epicLinkNumbers objects
        , _message $ OV.parentNumberFormat $ _parentEpicNumbers objects
        , _message $ OV.linking (_definitionEpicLinkNumbers objects) (_parentEpicLinkNumbers objects) -- todo operator
        ] of
        Success () -> createAll objects
        Failure errors -> printErrors errors

    Failure fms -> printErrors $ map toMessage fms
