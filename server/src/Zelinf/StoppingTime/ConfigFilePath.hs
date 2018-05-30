{-# LANGUAGE CPP #-}

module Zelinf.StoppingTime.ConfigFilePath
  ( fileName
  ) where

import           Data.Monoid ((<>))

fileName :: FilePath
fileName = "../config/" <>
#ifdef PRODUCTION
  "config.prod.json"
#else
  "config.dev.json"
#endif
