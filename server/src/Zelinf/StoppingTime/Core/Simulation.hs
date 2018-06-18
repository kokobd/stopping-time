{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zelinf.StoppingTime.Core.Simulation
  ( averageProfit
  , simulateOnce
  , Params(..)
  , safeHead
  ) where

import           Control.Monad.Random.Class
import           Control.Monad.State.Strict
import           Data.List                  (scanl')

averageProfit :: MonadRandom m
              => Params
              -> Int -- ^count
              -> m Double
averageProfit params n =
  fmap average . sequenceA . take n . repeat $ (simulateOnce params)
  where average xs = sum xs / fromIntegral n

data Params = Params
  { awards          :: [(Double, Double)]
  , devaluationRate :: Double
  , stopValue       :: Double
  }

simulateOnce :: MonadRandom m
             => Params
             -> m Double
simulateOnce params = execStateT (simulateOnce' params) 0

simulateOnce' :: MonadRandom m
              => Params
              -> StateT Double m ()
simulateOnce' Params{..} = do
  s <- get
  if s >= stopValue
    then pure ()
    else do
      (x :: Double) <- getRandomR (0, 1)
      let index = fmap fst . safeHead
               . dropWhile (\(_, p) -> p < x) . zip [0..] . drop 1
               . scanl' (+) 0 . fmap snd $ awards
      case index of
        Nothing  -> put 0
        (Just i) -> do
          modify (\s' -> s' * devaluationRate + fst (awards !! i))
          simulateOnce' $ Params awards devaluationRate stopValue

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
