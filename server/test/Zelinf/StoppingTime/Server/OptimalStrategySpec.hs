{-# LANGUAGE OverloadedLists #-}

module Zelinf.StoppingTime.Server.OptimalStrategySpec
  ( spec
  ) where

import           Data.Vector                              (Vector)
import           Test.Hspec

import           Zelinf.StoppingTime.Core.OptimalStrategy

spec :: Spec
spec = do
  describe "optimalStrategy" $ do
    it "works for one sample" $ do
      expectOptimalStrategy
        [1, 2, 3, 4, 5, 0]
        [1, 1, 1, 1, 1, 1]
        10
        [False, True, True, True, True, True]

expectOptimalStrategy :: [Double] -- ^f
                      -> [Double] -- ^g
                      -> Int -- ^iteration count
                      -> Vector Bool -- ^expected result
                      -> Expectation
expectOptimalStrategy f g n expected = do
  let strategyM = optimalStrategy f g n
  case strategyM of
    Nothing         -> expectationFailure "Nothing"
    (Just strategy) -> strategy `shouldBe` expected
