{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Zelinf.StoppingTime.Server.OptimalStrategy
  ( server
  ) where

import           Control.Monad.Except                     (throwError)
import           Servant.Server

import           Zelinf.StoppingTime.API.OptimalStrategy
import           Zelinf.StoppingTime.Core.OptimalStrategy

server :: Server API
-- server :: Params -> Handler (Strategy Double)
server Params{..} = do
  let resultM = optimalStrategy paramIncome paramCost paramIterations
  case resultM of
    Nothing       -> throwError err412
    (Just result) -> pure result
