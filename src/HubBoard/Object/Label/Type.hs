module HubBoard.Object.Label.Type (
    Label(..)
) where

import           Text.Printf                    ( printf )

newtype Label = Label String
instance Show Label where
    show (Label name) = printf "Label(%s)" name
