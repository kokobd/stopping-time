{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Zelinf.StoppingTime.Core.OptimalStrategy
  ( optimalStrategy
  ) where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.List            (genericIndex)
import           Data.Matrix          (Matrix)
import qualified Data.Matrix          as Matrix
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

optimalStrategy :: (Ord a, Fractional a, Foldable t, Integral i)
                => t a -- ^f: income(each turn) vector
                -> t a -- ^g: cost vector
                -> i -- ^iterations
                -> Maybe (Vector Bool)
optimalStrategy f' g' n =
  let f = toVector f'
      g = toVector g'
  in if | length f /= length g -> Nothing
        | length f == 0 -> Just Vector.empty
        | otherwise ->
            Just (optimalStrategy' f g n)

optimalStrategy' :: (Ord a, Fractional a, Integral i)
                 => Vector a -- ^f
                 -> Vector a -- ^g
                 -> i
                 -> Vector Bool
optimalStrategy' f g n =
  lastVectorToStrategy f . flip runReader environment $
    flip genericIndex (n-1) <$> (iterateM iterateWithCost initial)
  where
  initial = fmap (\x -> if x /= 0 then maxInF else 0) f
    where maxInF = maximum f
  environment = Environment g f (transitionMatrix f)

transitionMatrix :: (Num a, Ord a, Fractional b)
                 => Vector a
                 -> Matrix b
transitionMatrix f =
  Matrix.matrix n n elemGen
  where
  n = length f
  elemGen (x, _) = if f Vector.! (x - 1) == 0 then 0 else 1 / fromIntegral n

{-# INLINE toVector #-}
toVector :: Foldable t
         => t a -> Vector a
toVector = Vector.fromList . toList

lastVectorToStrategy :: Ord a
                     => Vector a -- ^f
                     -> Vector a -- ^u
                     -> Vector Bool
lastVectorToStrategy f u = Vector.zipWith (>=) f u

data Environment a = Environment
  { envCost             :: Vector a
  , envIncome           :: Vector a
  , envTransitionMatrix :: Matrix a
  }

iterateWithCost :: (Ord a, Num a, MonadReader (Environment a) m)
                => Vector a -- ^u_(n-1)
                -> m (Vector a) -- ^u_n
iterateWithCost u = do
  Environment{..} <- ask
  let mat = envTransitionMatrix
  let v1 = envIncome
  let v2 = Vector.zipWith (-) (matrixTimesVector mat u) envCost
  pure $ Vector.zipWith max v1 v2

rows :: Matrix a -> [Vector a]
rows mat = fmap (flip Matrix.getRow mat) [1..(Matrix.nrows mat)]

multiplyVector :: Num a => Vector a -> Vector a -> a
multiplyVector xs ys = Vector.sum (Vector.zipWith (*) xs ys)

matrixTimesVector :: Num a => Matrix a -> Vector a -> Vector a
matrixTimesVector mat vec =
  Vector.fromList $ fmap (multiplyVector vec) (rows mat)

iterateM :: Monad m
         => (a -> m a) -> a -> m [a]
iterateM f x = do
  y <- f x
  (x:) <$> (iterateM f y)
