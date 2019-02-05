module HubBoard.Object.Pipeline.Type (
    Pipeline(..)
) where

import           Text.Printf                    ( printf )

data Pipeline = Pipeline String String
instance Show Pipeline where
    show (Pipeline id name) = printf "Pipeline(%s)" name
