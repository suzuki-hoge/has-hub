module HubBoard.Object.Pipeline.Type (
    Pipeline(..)
) where

import           Text.Printf                    ( printf )

data Pipeline = Pipeline String String deriving Eq
instance Show Pipeline where
    show (Pipeline id name) = name
