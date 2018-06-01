module Zelinf.StoppingTime.Core.Util
  ( foldableToVector
  ) where

import           Data.Foldable (toList)
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector

{-# INLINE foldableToVector #-}
foldableToVector :: Foldable t
                 => t a -> Vector a
foldableToVector = Vector.fromList . toList
