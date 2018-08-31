module HasHub.Command.CreateObjects
(
  execute
)
where


import Text.Printf (printf)

import qualified HasHub.Command.ReferAll as RA
import qualified HasHub.Command.ValidateObjects as VO

import HasHub.Object.Object.Parser as Parser

import HasHub.Object.Object.Client as OC
import HasHub.Object.Pipeline.Client as PC
import HasHub.Object.Milestone.Client as MC

import HasHub.FixMe (printMessages, printFixMes, Validation(..))


execute :: [FilePath] -> IO ()
execute fps = do
  putStrLn "\ncreate objects."

  parsed <- Parser.readObjects fps

  case parsed of
    Success yamls -> do
      validated <- VO.execute' yamls

      case validated of
        Success (pipelines, milestones, epics) -> createAll yamls pipelines milestones epics
        Failure errors                         -> printMessages errors

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
