module HubBoard.Object.Collaborator.Type  (
    Collaborator(..)
) where

newtype Collaborator = Collaborator String deriving Eq
instance Show Collaborator where
    show (Collaborator name) = name
