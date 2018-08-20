module HasHub.Command.CreateObjects
(
  execute
)
where


import Text.Printf (printf)

import qualified HasHub.Command.ReferAll as RA

import HasHub.Object.Object.Parser as Parser

import HasHub.Object.Object.Client as OC
import HasHub.Object.Pipeline.Client as PC
import HasHub.Object.Milestone.Client as MC

import HasHub.Object.Object.Validator as OV
import HasHub.Object.Pipeline.Validator as PV
import HasHub.Object.Label.Validator as LV
import HasHub.Object.Collaborator.Validator as CV
import HasHub.Object.Milestone.Validator as MV

import HasHub.FixMe (flat, _message, printMessages, printFixMes, FixMe(..), Validation(..))


execute :: FilePath -> IO ()
execute yaml = do
  parsed <- Parser.readObjects yaml

  case parsed of
    Success(objects) -> do
      putStrLn "\nrefer all for yaml data validation."

      (epics, pipelines, labels, collaborators, milestones) <- RA.execute'

      case flat [
          _message $ _parentEpicNumbers objects `OV.areAllIn` epics
        , _message $ _pipelineNames objects `PV.areAllIn` pipelines
        , _message $ _labels objects `LV.areAllIn` labels
        , _message $ _collaborators objects `CV.areAllIn` collaborators
        , _message $ _milestoneTitles objects `MV.areAllIn` milestones
        , _message $ OV.noDuplication $ _epicLinkNumbersWithDuplication objects
        , _message $ OV.linkNumberFormat $ _epicLinkNumbers objects
        , _message $ OV.parentNumberFormat $ _parentEpicNumbers objects
        , _message $ OV.linking (_definitionEpicLinkNumbers objects) (_parentEpicLinkNumbers objects) -- todo operator
        ] of
        Success ()     -> createAll objects pipelines milestones epics
        Failure errors -> printMessages errors

    Failure fms -> printFixMes fms


type Current = Int
type Total = Int
type ReferredEpic = Epic
type ParentEpic = Epic


createAll :: [YamlObject] -> [Pipeline] -> [Milestone] -> [ReferredEpic] -> IO ()
createAll objects = createAll' (length objects) [] objects
  where
    createAll' :: Int -> [LinkedEpic] -> [YamlObject] -> [Pipeline] -> [Milestone] -> [ReferredEpic] -> IO ()
    createAll' total _           []               _         _          _     = printf "\n%d objects created.\n" total
    createAll' total linkedEpics (object:objects) pipelines milestones epics = do
      let pipeline = _pipelineName object >>= PC.findIn pipelines
      let milestone = _milestoneTitle object >>= MC.findIn milestones
      let parentEpics = _parentEpicNumber object >>= OC.findIn linkedEpics epics

      linkedEpic <- create object pipeline milestone parentEpics (total - length objects) total

      createAll' total (linkedEpic ++ linkedEpics) objects pipelines milestones epics
      where
        create :: YamlObject -> Maybe Pipeline -> Maybe Milestone -> [ParentEpic] -> Current -> Total -> IO [LinkedEpic]
        create (EpicYamlObject epicLinkNumber title body _ labels collaborators _ estimate _) pipeline milestone epics current total = do
          printf "\ncreate Epic. (%d / %d)\n" current total

          (:[]) <$> OC.createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epics

        create (IssueYamlObject title body _ labels collaborators _ estimate _) pipeline milestone epics current total = do
          printf "\ncreate Issue. (%d / %d)\n" current total

          const [] <$> OC.createIssue title body pipeline labels collaborators milestone estimate epics
