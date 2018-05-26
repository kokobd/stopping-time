{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Zelinf.StoppingTime.Server.Strategy
  ( Strategy
  , shouldStop
  , stopValues
  , fromList
  ) where

import           Data.Aeson
import           Data.Set   (Set)
import qualified Data.Set   as Set

{-|
A strategy describes when to stop. It is basically a
stopping set.
Its JSON format is guranteed to be a JSON array.
-}
newtype Strategy a = Strategy (Set a)
  deriving (ToJSON, FromJSON, Eq)

instance Show a => Show (Strategy a) where
  show = show . stopValues

shouldStop :: Ord a => Strategy a -> a -> Bool
shouldStop (Strategy xs) x = Set.member x xs

stopValues :: Strategy a -> [a]
stopValues (Strategy xs) = Set.elems xs

fromList :: Ord a => [a] -> Strategy a
fromList = Strategy . Set.fromList
