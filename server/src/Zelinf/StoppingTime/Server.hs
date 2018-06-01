{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Zelinf.StoppingTime.Server
  ( main
  , app
  , server
  ) where

import           Data.Proxy
import           Network.Wai.Handler.Warp                   (run)
import           Network.Wai.Middleware.Cors
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles                  (serveDirectoryWebApp)

import           Zelinf.StoppingTime.API                    (API)
import           Zelinf.StoppingTime.Config                 (config)
import qualified Zelinf.StoppingTime.Config                 as Config
import qualified Zelinf.StoppingTime.Server.OptimalStrategy as OptimalStrategy
import qualified Zelinf.StoppingTime.Server.Simulation      as Simulation

main :: IO ()
main = do
  let port = fromIntegral $ Config.port config
  putStrLn $ "Hosting on port " ++ show port
  run port app

app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API') server'

server :: Server API
server =
       OptimalStrategy.server
  :<|> Simulation.server

server' :: Server API'
server' = server :<|> staticServer

type API' = API
  :<|> StaticAPI

type StaticAPI = Raw

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "static"

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let requestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
  in simpleCorsResourcePolicy { corsRequestHeaders = requestHeaders }
