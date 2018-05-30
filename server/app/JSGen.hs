{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Servant.JS

import           Zelinf.StoppingTime.API
import           Zelinf.StoppingTime.Config (config)
import qualified Zelinf.StoppingTime.Config as Config

main :: IO ()
main = do
  Text.putStrLn $
    jsForAPI (Proxy :: Proxy API)
    (jqueryWith
      defCommonGeneratorOptions{
        moduleName = "exports",
        urlPrefix = theUrlPrefix
      })

theUrlPrefix :: Text
theUrlPrefix =
     "http://" <> (Config.ip config)
  <> ":" <> Text.pack (show (Config.port config))
