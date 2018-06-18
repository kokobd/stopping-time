{-# LANGUAGE OverloadedLists #-}

module Zelinf.StoppingTime.Core.OptimalStrategySpec
  ( spec
  ) where

import           Test.Hspec

import           Zelinf.StoppingTime.Core.OptimalStrategy

spec :: Spec
spec = do
  describe "optimalStrategy" $ do
    it "works for one sample" $ do
      let p = 1/6
      let (Just v) = (optimalStrategy [(1, p), (2, p), (3, p), (4, p), (5, p)] 1)
      doubleEqual 0.01 15 v
        `shouldBe` True

doubleEqual :: Double -> Double -> Double -> Bool
doubleEqual r x y = x - y < r
