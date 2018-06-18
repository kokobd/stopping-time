{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Zelinf.StoppingTime.API.OptimalStrategy
  ( API
  , Params(..)
  , Result(..)
  ) where

import           Data.Aeson                        hiding (Result)
import           Data.Aeson.Types                  (fieldLabelModifier)
import           GHC.Generics
import           Servant.API
import           Servant.Docs                      (ToSample, singleSample,
                                                    toSamples)
import           Zelinf.StoppingTime.Internal.Util (removeFirstWordCamelCase)

type API = "optimal-strategy"
         :> ReqBody '[JSON] Params :> Post '[JSON] Result

data Params = Params
  { paramAwards          :: [(Double, Double)]
  , paramDevaluationRate :: Double
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
  toSamples _ = singleSample $ Params
    [(1, 0.2), (2, 0.3), (4, 0.2)] 1.0

instance ToSample Result where
  toSamples _ = singleSample $ Result
    2.3
