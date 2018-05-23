{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Zelinf.Markov
  (
  ) where

import           Control.Monad.Reader.Class
import           Data.Matrix                (Matrix)
import qualified Data.Matrix                as Matrix
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector

data Condition a = Condition
  { conditionIncome   :: Vector a
  , conditionTransMat :: Matrix a
  }

-- iteratePlain :: (Num a, MonadReader (Condition a) m)
--              => Vector a -> m (Vector a)
-- iteratePlain = undefined

iterateWithCost :: (Ord a, Num a, MonadReader m, (EnvType m ~ Condition a))
                => Vector a -- ^cost
                -> Vector a -- ^u_(n-1)
                -> m (Vector a) -- ^u_n
iterateWithCost cost u = do
  Condition{..} <- ask
  let mat = conditionTransMat
  let v1 = conditionIncome
  let v2 = Vector.zipWith (-) (matrixTimesVector mat u) cost
  pure $ Vector.zipWith max v1 v2

-- iterateWithDevalue :: (Num a, MonadReader (Condition a) m)
--                    => Vector a -> m (Vector a)
-- iterateWithDevalue = undefined

rows :: Matrix a -> [Vector a]
rows mat = fmap (flip Matrix.getRow mat) [1..(Matrix.nrows mat)]

multiplyVector :: Num a => Vector a -> Vector a -> a
multiplyVector xs ys = Vector.sum (Vector.zipWith (*) xs ys)

matrixTimesVector :: Num a => Matrix a -> Vector a -> Vector a
matrixTimesVector mat vec =
  Vector.fromList $ fmap (multiplyVector vec) (rows mat)
