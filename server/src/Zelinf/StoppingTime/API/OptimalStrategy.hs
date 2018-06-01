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
import           Data.Vector                       (Vector)
import           GHC.Generics
import           Servant.API
import           Servant.Docs                      (ToSample, singleSample,
                                                    toSamples)
import           Zelinf.StoppingTime.Internal.Util (removeFirstWordCamelCase)

type API = "optimal-strategy"
         :> ReqBody '[JSON] Params :> Post '[JSON] Result

data Params = Params
  { paramIncome     :: Vector Double
  , paramCost       :: Vector Double
  , paramIterations :: Int
  } deriving Generic

newtype Result = Result (Vector Bool)
  deriving (ToJSON, FromJSON)

instance FromJSON Params where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToJSON Params where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = removeFirstWordCamelCase }

instance ToSample Params where
  toSamples _ = singleSample (Params [1, 2, 3, 4, 5, 0] [1, 1, 1, 1, 1, 1] 10)

instance ToSample Result where
  toSamples _ = singleSample $ Result [False, True, True, True, True, True]
