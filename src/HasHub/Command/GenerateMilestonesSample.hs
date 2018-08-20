module HasHub.Command.GenerateMilestonesSample
(
  execute
)
where


import HasHub.Object.Milestone.Parser as Parser


execute :: FilePath -> IO ()
execute out = do
  putStrLn "\ngenerate milestones sample."
  putStrLn $ "  write " ++ out

  writeFile out $ unlines [
      "- title    : sprint 1"
    , "  start_on : 2018-01-01"
    , "  due_on   : 2018-01-31"
    , ""
    , "- title    : sprint 2"
    ]

  putStrLn "\ngenerated."
