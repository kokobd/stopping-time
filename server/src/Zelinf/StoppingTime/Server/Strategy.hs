{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Zelinf.StoppingTime.Server.Strategy
  ( Strategy
  , shouldStop
  ) where

import           Data.Aeson
import           Data.List
import           GHC.Exts

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

instance IsList (Strategy a) where
  type Item (Strategy a) = a
  fromList = Strategy
  toList (Strategy xs) = xs
