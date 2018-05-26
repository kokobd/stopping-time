{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Zelinf.StoppingTime.Server.Strategy
  ( Strategy
  , shouldStop
  , fromList
  ) where

import           Data.Aeson
import           Data.List

{-|
A strategy describes when to stop. It is basically a
stopping set.
Its JSON format is guranteed to be a JSON array.
-}
newtype Strategy a = Strategy [a]
  deriving (ToJSON, FromJSON, Show)

instance Ord a => Eq (Strategy a) where
  (Strategy xs) == (Strategy ys) = (sort . nub $ xs) == (sort . nub $ ys)

shouldStop :: Eq a => Strategy a -> a -> Bool
shouldStop (Strategy xs) x = elem x xs

fromList :: [a] -> Strategy a
fromList xs = Strategy xs
