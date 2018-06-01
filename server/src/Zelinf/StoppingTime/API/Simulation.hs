{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeOperators              #-}

module Zelinf.StoppingTime.API.Simulation
  ( API
  , Params(..)
  , Result(..)
  ) where

import           Data.Aeson                        hiding (Result)
import           Data.Aeson.Types                  (fieldLabelModifier)
import           Data.Vector                       (Vector)
import           GHC.Generics                      (Generic)
import           Servant.API
import           Servant.Docs                      (ToSample, singleSample,
                                                    toSamples)
import           Zelinf.StoppingTime.Internal.Util (removeFirstWordCamelCase)

type API = "simulation" :> ReqBody '[JSON] Params :> Post '[JSON] Result

data Params = Params
  { paramIncome :: Vector Double
  , paramCost   :: Vector Double
  , paramStop   :: Vector Bool
  , paramCount  :: Int
  } deriving Generic

newtype Result = Result Double
  deriving (ToJSON, FromJSON)

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToJSON Params where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToSample Params where
  toSamples _ = singleSample params
    where params = Params [1, 2, 3, 4, 5, 0]
                          [1, 1, 1, 1, 1, 1]
                          [False, True, True, True, True, True]
                          2

instance ToSample Result where
  toSamples _ = singleSample (Result 1.52)
