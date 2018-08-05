module HasHub.Client.Fetcher where


import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.ByteString.Lazy.Internal as L (ByteString)


data Fetcher = Fetcher (Request -> IO L.ByteString)
instance Show Fetcher where
  show _ = "fetcher"

fetcher = Fetcher $ \request -> do
  manager <- newManager tlsManagerSettings
  responseBody <$> httpLbs request manager
