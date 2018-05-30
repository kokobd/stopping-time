{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Zelinf.StoppingTime.Config
  ( Config
  , config
  , ip
  , port
  ) where

import           Data.Aeson
import           Data.ByteString                    (ByteString)
import           Data.FileEmbed                     (embedFile,
                                                     makeRelativeToProject)
import           Data.Maybe                         (fromJust)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Word                          (Word16)
import           GHC.Generics

import           Zelinf.StoppingTime.ConfigFilePath

data Config = Config
  { ip   :: Text
  , port :: Word16
  } deriving Generic

instance FromJSON Config

configFile :: ByteString
configFile =
  $(makeRelativeToProject fileName >>= embedFile)

config :: Config
config = fromJust $ decodeStrict configFile
