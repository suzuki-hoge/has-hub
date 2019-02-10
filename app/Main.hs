{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HubBoard.Fetcher     as F
import           HubBoard.Yaml.Parser
import           HubBoard.Object.Epic as E

main :: IO ()
main = run "test/yamls/full-parameters.yaml"

run :: FilePath -> IO ()
run fp = do
    F.initialize

    validated <- parse fp

    case validated of
        (Right epics) -> do
            putStrLn "\ncreating..."
            mapM_ E.create epics
            putStrLn "\ncompleted.\n"
        (Left es) -> do
            putStrLn "\nfix following validation errors!!!"
            mapM_ putStrLn es
            putStrLn "\naboted.\n"
