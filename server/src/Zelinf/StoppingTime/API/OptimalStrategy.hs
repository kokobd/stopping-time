{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Zelinf.StoppingTime.API.OptimalStrategy
  ( API
  , Params(..)
  ) where

import           Data.Aeson
import           Data.Vector                       (Vector)
import           Servant.API
import           Zelinf.StoppingTime.Core.Strategy

type API = "optimal-strategy"
         :> ReqBody '[JSON] Params :> Get '[JSON] Strategy

data Params = Params
  { paramIncome     :: Vector Double
  , paramCost       :: Vector Double
  , paramIterations :: Int
  }

instance FromJSON Params where
  parseJSON = withObject "Params" $ \v -> Params
    <$> v .: "income"
    <*> v .: "cost"
    <*> v .: "iterations"
