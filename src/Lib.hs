module Lib
    ( someFunc
    )
where

import           HubBoard.Object.Label.Mapper
import           HubBoard.Transfer.Transfer
import           HubBoard.Transfer.Config

import System.Directory

someFunc :: IO ()
someFunc = do
    validation <- validate
    print validation

    labels <- getGitHub refer
    print labels
    