{-# LANGUAGE RecordWildCards #-}

module Zelinf.StoppingTime.Server.Simulation
  ( server
  ) where

import           Control.Monad.Except                (throwError)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Maybe                          (maybe)
import           Servant.Server

import           Zelinf.StoppingTime.API.Simulation  (API)
import qualified Zelinf.StoppingTime.API.Simulation  as API
import qualified Zelinf.StoppingTime.Core.Simulation as Core

server :: Server API
server (API.Params awards dr sv count) = do
  result <- liftIO $ Core.averageProfit (Core.Params
      awards
      dr
      sv
      ) count
  pure . API.Result $ result
