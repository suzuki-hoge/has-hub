module Lib
    ( someFunc
    )
where

import           HubBoard.Object.Label.Mapper
import           HubBoard.Transfer.Transfer

someFunc :: IO ()
someFunc = do
    labels <- getGitHub mapper
    print labels
