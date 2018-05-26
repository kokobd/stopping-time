module Zelinf.StoppingTime.Server.Simulation
  (
  ) where

import           Data.Vector                         (Vector)

import           Zelinf.StoppingTime.Server.Strategy

averageIncome :: (Fractional a, Foldable t, Integral i)
              => t a -- ^f
              -> t a -- ^g
              -> i -- ^simulation times
              -> Strategy a -- ^the strategy of choice
              -> Maybe a -- ^average profit
averageIncome = undefined

