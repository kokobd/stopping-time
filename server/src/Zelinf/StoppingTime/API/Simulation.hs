{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Zelinf.StoppingTime.API.Simulation
  ( API
  , Params(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types (fieldLabelModifier)
import           Data.Vector      (Vector)
import           GHC.Generics     (Generic)
import           Servant.API

type API = "simulation" :> ReqBody '[JSON] Params :> Post '[JSON] Double

data Params = Params
  { paramIncome :: Vector Double
  , paramCost   :: Vector Double
  , paramStop   :: Vector Bool
  , paramCount  :: Int
  } deriving Generic

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop n }
    where n = length "param"
