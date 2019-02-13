module HubBoard.Command.New(
    exec
) where

import           System.IO        ( hFlush, stdout )
import           System.Directory ( createDirectory )
import           Text.Printf      ( printf )

exec :: String -> IO ()
exec name = do
    putStrLn "\nsetup destination board config"
    putStrLn "  input owner and repository."
    putStrLn "\n  ex: when url is \"https://github.com/suzuki-hoge/project-a\", owner is \"suzuki-hoge\", repository is \"project-a\"."
    putStr "\n  owner: "
    owner <- withEchoGetLine
    putStr "  repository: "
    repository <- withEchoGetLine

    let workspace = printf "./%s" name
    createDirectory workspace

    let configPath = printf "%s/.hub-board-config.yaml" workspace
    putStrLn "\nwrite config"
    putStrLn $ "  " ++ configPath
    writeFile configPath $ printf "owner: %s\nrepository: %s\n" owner repository

    let yamlPath = printf "%s/sprint-1.yaml" workspace
    putStrLn "\nwrite sample yaml"
    putStrLn $ "  " ++ yamlPath

    yamlPath > "milestone:"
    yamlPath > "    new-milestone:"
    yamlPath > "        title   : sprint 1"
    yamlPath > "        start-on: 2019-04-01"
    yamlPath > "        due-on  : 2019-04-12"
    yamlPath > ""
    yamlPath > "default-pipeline: sprint backlog"
    yamlPath > ""
    yamlPath > "epics:"
    yamlPath > "  - new-epic: "
    yamlPath > "      title    : epic title 1"
    yamlPath > "      body     : epic body 1"
    yamlPath > "      labels   : [dev]"
    yamlPath > "      assignees: [suzuki-hoge]"
    yamlPath > "      pipeline : backlog"
    yamlPath > "      estimate : 5"
    yamlPath > ""
    yamlPath > "      issues: "
    yamlPath > "        - title    : issue title 1"
    yamlPath > "          body     : issue body 1"
    yamlPath > "          labels   : [setup]"
    yamlPath > "          assignees: [suzuki-hoge]"
    yamlPath > "          pipeline : sprint backlog"
    yamlPath > "          estimate : 3"
    yamlPath > ""
    yamlPath > "  - existing-epic: "
    yamlPath > "      number   : 1"
    yamlPath > ""
    yamlPath > "      issues: "
    yamlPath > "        - title    : issue title 2"
    yamlPath > "        - title    : issue title 3"
    yamlPath > ""
    yamlPath > "  - no-epic: "
    yamlPath > "      issues: "
    yamlPath > "        - title    : issue title 4"
    yamlPath > "          body     : |"
    yamlPath > "            # issue body 4"
    yamlPath > "            + [ ] task 1"
    yamlPath > "            + [ ] task 2"

    putStrLn "\ncompleted."
  where
    withEchoGetLine :: IO String
    withEchoGetLine = do
        hFlush stdout
        value <- getLine
        putChar '\n'
        return value

    path > line = appendFile path $ line ++ "\n"