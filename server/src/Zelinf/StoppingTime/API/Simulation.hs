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
import           GHC.Generics                      (Generic)
import           Servant.API
import           Servant.Docs                      (ToSample, singleSample,
                                                    toSamples)
import           Zelinf.StoppingTime.Internal.Util (removeFirstWordCamelCase)

type API = "simulation" :> ReqBody '[JSON] Params :> Post '[JSON] Result

newtype Result = Result Double
  deriving (ToJSON, FromJSON)

data Params = Params
  { paramAwards          :: [(Double, Double)]
  , paramDevaluationRate :: Double
  , paramStopValue       :: Double
  , paramCount           :: Int
  } deriving Generic

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToJSON Params where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToSample Params where
  toSamples _ = singleSample $ Params
    [(1, 0.2), (2, 0.3), (4, 0.2)] 1.0 15.0 10000

instance ToSample Result where
  toSamples _ = singleSample $ Result
    4.5
