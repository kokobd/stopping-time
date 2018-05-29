{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Zelinf.StoppingTime.API.OptimalStrategy
  ( API
  , Params(..)
  ) where

import           Data.Aeson
import           Data.Vector (Vector)
import           Servant.API

type API = "optimal-strategy"
         :> ReqBody '[JSON] Params :> Post '[JSON] (Vector Bool)

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
