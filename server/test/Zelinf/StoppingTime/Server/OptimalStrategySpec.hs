{-# LANGUAGE OverloadedLists #-}

module Zelinf.StoppingTime.Server.OptimalStrategySpec
  ( spec
  ) where

import           Test.Hspec

import           Zelinf.StoppingTime.Server.OptimalStrategy
import           Zelinf.StoppingTime.Server.Strategy

spec :: Spec
spec = do
  describe "optimalStrategy" $ do
    it "works for one sample" $ do
      expectOptimalStrategy
        [1, 2, 3, 4, 5, 0]
        [1, 1, 1, 1, 1, 1]
        10
        (fromList [2, 3, 4, 5, 0])

expectOptimalStrategy :: [Double] -- ^f
                      -> [Double] -- ^g
                      -> Int -- ^iteration count
                      -> Strategy Double -- ^expected result
                      -> Expectation
expectOptimalStrategy f g n expected = do
  let strategyM = optimalStrategy f g n
  case strategyM of
    Nothing         -> expectationFailure "Nothing"
    (Just strategy) -> strategy `shouldBe` expected
