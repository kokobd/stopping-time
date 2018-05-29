module Zelinf.StoppingTime.Core.Simulation
  (
  ) where

import           Data.Vector (Vector)

averageIncome :: (Fractional a, Foldable t, Integral i)
              => t a -- ^f
              -> t a -- ^g
              -> i -- ^simulation times
              -> t Bool -- ^the strategy of choice
              -> Maybe a -- ^average profit
averageIncome = undefined

