module HubBoard.Object.Label.Type (
    Label(..)
) where

newtype Label = Label String deriving Eq
instance Show Label where
    show (Label name) = name
