module HubBoard.Object.Epic.Type
    ( Epic(..)
    )
where

import           Text.Printf                    ( printf )

data Epic = Epic Int String
instance Show Epic where
    show (Epic number title) = printf "Epic([#%d] %s)" number title
