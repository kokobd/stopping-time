{-# LANGUAGE RecordWildCards #-}

module Zelinf.StoppingTime.Server.Simulation
  ( server
  ) where

import           Control.Monad.Except                (throwError)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Maybe                          (maybe)
import           Servant.Server
import           Zelinf.StoppingTime.API.Simulation
import           Zelinf.StoppingTime.Core.Simulation

server :: Server API
server Params{..} = do
  result <- liftIO $ averageProfit paramIncome paramCost paramStop paramCount
  maybe (throwError err412) pure result
