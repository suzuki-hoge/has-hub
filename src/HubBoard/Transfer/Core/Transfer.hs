module HubBoard.Transfer.Core.Transfer where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy.Internal as LBS


secureFetch :: Request -> IO LBS.ByteString
secureFetch request = do
    manager  <- newManager tlsManagerSettings

    response <- responseBody <$> httpLbs request manager

    return response
