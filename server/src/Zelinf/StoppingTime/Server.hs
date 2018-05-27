module Zelinf.StoppingTime.Server
  ( main
  , app
  , server
  ) where

import           Data.Proxy
import           Network.Wai.Handler.Warp                   (run)
import           Servant.Server

import           Zelinf.StoppingTime.API                    (API)
import qualified Zelinf.StoppingTime.Server.OptimalStrategy as OptimalStrategy

main :: IO ()
main = run 8080 app

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = OptimalStrategy.server
