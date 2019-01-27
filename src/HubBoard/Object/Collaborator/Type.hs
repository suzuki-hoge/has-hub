module HubBoard.Object.Collaborator.Type
    ( Collaborator(..)
    )
where

import           Text.Printf                    ( printf )

newtype Collaborator = Collaborator String
instance Show Collaborator where
    show (Collaborator name) = printf "Collaborator(%s)" name
