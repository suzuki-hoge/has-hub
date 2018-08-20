module HasHub.Command.GenerateObjectsSample
(
  execute
)
where


import HasHub.Object.Object.Parser as Parser


execute :: FilePath -> IO ()
execute out = do
  putStrLn "\ngenerate objects sample."
  putStrLn $ "  write " ++ out

  writeFile out $ unlines [
      "- epic-link-number : ?1"
    , "  title            : foo epic title"
    , ""
    , "- title            : bar issue title"
    , "  body             : bar issue body"
    , "  pipeline         : bar backlog"
    , "  labels           : [label-1, label-2]"
    , "  assignees        : [assignees-1]"
    , "  milestone        : milestone-1"
    , "  estimate         : 3"
    , "  epics            : [\"?1\", \"#2\"]"
    ]

  putStrLn "\ngenerated."
