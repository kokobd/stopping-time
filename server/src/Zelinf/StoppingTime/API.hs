{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Zelinf.StoppingTime.API
  ( API
  ) where

import           Servant.API
import qualified Zelinf.StoppingTime.API.OptimalStrategy as OptimalStrategy

type API = "api" :> OptimalStrategy.API
