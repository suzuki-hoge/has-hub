module HubBoard.Object.Pipeline.Type
    ( Pipeline(..)
    )
where

import           Text.Printf                    ( printf )

data Pipeline = Pipeline String String
instance Show Pipeline where
    show (Pipeline _ name) = printf "Pipeline(%s)" name
