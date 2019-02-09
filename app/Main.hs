{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HubBoard.Fetcher             as F
import           HubBoard.Yaml.Parser

main :: IO ()
main = do
    F.initialize
    -- validated <- parse "test/yamls/full-parameters.yaml"
    validated <- parse "test/yamls/epic-patterns/no-epic.yaml"
    case validated of
        (Right epics) -> print epics
        (Left es) -> do
            putStrLn "\nfix following validation errors!!!"
            mapM_ putStrLn es
    return ()
