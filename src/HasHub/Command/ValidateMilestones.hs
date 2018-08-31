module HasHub.Command.ValidateMilestones
(
  execute
, execute'
)
where


import HasHub.Object.Milestone.Parser as Parser
import HasHub.Object.Milestone.Validator as MV
import HasHub.Object.Milestone.Client as MC

import HasHub.FixMe (flat, _message, printMessages, printFixMes, Validation(..))


execute :: [FilePath] -> IO ()
execute fps = do
  putStrLn "\nvalidate all milestones."

  parsed <- Parser.readObjects fps

  case parsed of
    Success yamls -> do
      validated <- execute' yamls

      case validated of
        Failure errors -> printMessages errors
        Success _      -> return ()

    Failure fms -> printFixMes fms

  putStrLn "\nall validation succeeded."


execute' :: [YamlMilestone] -> IO (Validation [String] ())
execute' yamls = do
  putStrLn "\nrefer milestones for yaml data validation."

  milestones    <- MC.referAll

  case flat [
      _message $ _titles yamls `MV.areAllNotIn` milestones
    , _message $ MV.noDuplication $ _titles yamls               -- todo spec
    , _message $ MV.startOnFormat $ _startOns yamls
    , _message $ MV.dueOnFormat $ _dueOns yamls
    ] of
    Success ()     -> return $ Success ()
    Failure errors -> return $ Failure errors
