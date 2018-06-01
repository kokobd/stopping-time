module Zelinf.StoppingTime.Internal.Util
  ( removeFirstWordCamelCase
  , foldableToVector
  , iterateM
  ) where

import           Data.Char     (isUpper, toLower)
import           Data.Foldable (toList)
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector

removeFirstWordCamelCase :: String -> String
removeFirstWordCamelCase str =
  case dropWhile (not . isUpper) str of
    []     -> []
    (x:xs) -> toLower x : xs

{-# INLINE foldableToVector #-}
foldableToVector :: Foldable t
                 => t a -> Vector a
foldableToVector = Vector.fromList . toList

iterateM :: Monad m
         => (a -> m a) -> a -> m [a]
iterateM f x = do
  y <- f x
  (x:) <$> (iterateM f y)
