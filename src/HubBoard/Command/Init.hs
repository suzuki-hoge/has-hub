module HubBoard.Command.Init (
    exec
) where

import           System.IO         ( hFlush, hGetEcho, hSetEcho, stdin, stdout )
import           Control.Exception ( bracket_ )
import           System.Directory  ( getHomeDirectory )
import           Text.Printf       ( printf )

exec :: IO ()
exec = do
    putStrLn "\ngithub token"
    putStrLn "  create github token with \"public_repo\" scope."
    putStrLn "\n  url: https://github.com/settings/tokens/new"
    putStr "\n  input github token ( password prompt ): "
    gitHubToken <- noEchoGetLine
    putStrLn $ "\n  got: " ++ mask gitHubToken

    putStrLn "\nzenhub token"
    putStrLn "  create zenhub token."
    putStrLn "\n  url: https://app.zenhub.com/dashboard/tokens"
    putStr "\n  input zenhub token ( password prompt ): "
    zenHubToken <- noEchoGetLine
    putStrLn $ "\n  got: " ++ mask zenHubToken

    path <- (++ "/.hub-board-config.yaml2") <$> getHomeDirectory
    putStrLn "\nwrite config"
    putStrLn $ "  " ++ path
    writeFile path $ printf "git-hub-token: %s\nzen-hub-token: %s\n" gitHubToken zenHubToken

    putStrLn "\ncompleted."
  where
    noEchoGetLine :: IO String
    noEchoGetLine = do
        hFlush stdout
        old <- hGetEcho stdin
        value <- bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getLine
        putChar '\n'
        return value

    mask :: String -> String
    mask s = head ++ map (const '.') remains' ++ reverse tail'
      where
        (head, remains) = splitAt 2 s
        (tail', remains') = splitAt 2 $ reverse remains