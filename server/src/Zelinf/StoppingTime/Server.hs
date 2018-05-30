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
import           Zelinf.StoppingTime.Config                 (Config, config)
import qualified Zelinf.StoppingTime.Config                 as Config
import qualified Zelinf.StoppingTime.Server.OptimalStrategy as OptimalStrategy

main :: IO ()
main = do
  let port = fromIntegral $ Config.port config
  putStrLn $ "Hosting on port " ++ show port
  run port app

app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API') server

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let requestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
  in simpleCorsResourcePolicy { corsRequestHeaders = requestHeaders }

type API' = API
  :<|> StaticAPI

server :: Server API'
server = OptimalStrategy.server
  :<|> staticServer

type StaticAPI = Raw

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "static"
