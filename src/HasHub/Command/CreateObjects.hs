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
    Success yamls -> do
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
        Success ()     -> createAll yamls pipelines milestones epics
        Failure errors -> printMessages errors

    Failure fms -> printFixMes fms


type Current = Int
type Total = Int
type ReferredEpic = Epic
type LinkingEpic = Epic


createAll :: [YamlObject] -> [Pipeline] -> [Milestone] -> [ReferredEpic] -> IO ()
createAll yamls = createAll' (length yamls) [] yamls
  where
    createAll' :: Int -> [LinkedEpic] -> [YamlObject] -> [Pipeline] -> [Milestone] -> [ReferredEpic] -> IO ()
    createAll' total _           []               _         _          _     = printf "\n%d objects created.\n" total
    createAll' total linkedEpics (yaml:yamls) pipelines milestones referreds = do
      let pipeline = _pipelineName yaml >>= PC.findIn pipelines
      let milestone = _milestoneTitle yaml >>= MC.findIn milestones
      let linkings = _linkingEpicNumber yaml >>= OC.findIn linkedEpics referreds

      linkedEpic <- create yaml pipeline milestone linkings (total - length yamls) total

      createAll' total (linkedEpic ++ linkedEpics) yamls pipelines milestones referreds
      where
        create :: YamlObject -> Maybe Pipeline -> Maybe Milestone -> [LinkingEpic] -> Current -> Total -> IO [LinkedEpic]
        create (YamlEpic epicLinkNumber title body _ labels collaborators _ estimate _) pipeline milestone linkings current total = do
          printf "\ncreate Epic. (%d / %d)\n" current total

          (:[]) <$> OC.createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate linkings

        create (YamlIssue title body _ labels collaborators _ estimate _) pipeline milestone linkings current total = do
          printf "\ncreate Issue. (%d / %d)\n" current total

          const [] <$> OC.createIssue title body pipeline labels collaborators milestone estimate linkings
