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
server Params{..} = do
  maybe (throwError err412) (pure . Result)
    (optimalStrategy paramAwards paramDevaluationRate)
