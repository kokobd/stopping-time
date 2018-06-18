{-# LANGUAGE MultiWayIf #-}

module Zelinf.StoppingTime.Core.OptimalStrategy
  ( optimalStrategy
  ) where

optimalStrategy :: [(Double, Double)] -- ^[(value, probability)]
                -> Double -- ^Devaluation rate per turn. 1 means no devaluation
                -> Maybe Double -- ^Should stop when s >= this value
optimalStrategy xs dr =
  let psum = sum (fmap snd xs)
      p = 1 - dr * psum
  in if | psum < 0 || psum > 1 -> Nothing
        | not $ and (fmap ((\x -> x>=0 && x<= 1) . snd) xs) -> Nothing
        | dr > 1 || dr < 0 -> Nothing
        | otherwise -> Just $ sum (fmap (uncurry (*)) xs) / p
