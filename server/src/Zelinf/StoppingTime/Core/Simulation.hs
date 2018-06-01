{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Zelinf.StoppingTime.Core.Simulation
  ( averageProfit
  , simulateOnce
  ) where

import           Control.Monad.Random.Class
import           Control.Monad.Reader
import           Data.List                         (span)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Zelinf.StoppingTime.Internal.Util (foldableToVector)

averageProfit :: (Fractional a, Foldable t, Integral i, MonadRandom m)
              => t a -- ^f, the income vector
              -> t a -- ^g, the cost vector
              -> t Bool -- ^the stopping set of choice
              -> i -- ^simulation times
              -> m (Maybe a) -- ^average profit
averageProfit f g s n =
  let f' = foldableToVector f
      g' = foldableToVector g
      s' = foldableToVector s
      n' = fromIntegral n
   in if | length f' /= length g' || length f' /= length s' || length f' == 0 ->
           pure Nothing
         | n' <= 0 -> pure Nothing
         | otherwise ->
           Just <$> runReaderT (averageProfit' n')
             (Input f' g' s')

data Input a = Input
  { inputIncome :: Vector a
  , inputCost   :: Vector a
  , inputStop   :: Vector Bool
  }

averageProfit' :: (Fractional a, MonadRandom m)
               => Int
               -> ReaderT (Input a) m a
averageProfit' n = fmap average (sequenceA $ take n (repeat simulateOnce))
  where average xs = sum xs / fromIntegral n

simulateOnce :: (Fractional a, MonadRandom m)
             => ReaderT (Input a) m a
simulateOnce = do
  Input{..} <- ask
  let n = length inputIncome
  randomIndexes <- getRandomRs (0, n - 1)
  let (xs', y':_) = span (\i -> not (inputStop Vector.! i)) randomIndexes
  let indexes = xs' ++ [y']
  pure $
    sum $ fmap (\i -> (inputIncome Vector.! i) - (inputCost Vector.! i)) indexes
