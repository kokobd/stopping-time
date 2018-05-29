{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Zelinf.StoppingTime.Server
  ( main
  , app
  , server
  ) where

import           Data.Proxy
import           Network.Wai.Handler.Warp                   (run)
import           Network.Wai.Middleware.Cors
import           Servant.Server

import           Zelinf.StoppingTime.API                    (API)
import qualified Zelinf.StoppingTime.Server.OptimalStrategy as OptimalStrategy

port :: Int
port =
#ifdef PRODUCTION
  80
#else
  8080
#endif

main :: IO ()
main = do
  putStrLn $ "Hosting on port " ++ show port
  run port app

app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API) server

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let requestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
  in simpleCorsResourcePolicy { corsRequestHeaders = requestHeaders }

server :: Server API
server = OptimalStrategy.server
