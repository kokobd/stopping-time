{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid             ((<>))
import           Data.Proxy              (Proxy (..))
import           Data.String             (fromString)
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Options.Applicative
import           Servant.JS

import           Zelinf.StoppingTime.API

main :: IO ()
main = do
  args <- execParser parserInfo
  Text.putStrLn $
    jsForAPI (Proxy :: Proxy API)
    (jqueryWith
      defCommonGeneratorOptions{
        moduleName = "exports",
        urlPrefix = argUrlPrefix args
      })

data Args = Args
  { argUrlPrefix :: Text
  }

argsParser :: Parser Args
argsParser = Args . fromString
  <$> strOption
    ( long "url-prefix"
    <> short 'p'
    <> metavar "PREFIX"
    <> help "resulting URL will be ${PREFIX}other"
    )

parserInfo :: ParserInfo Args
parserInfo = info (argsParser <**> helper) fullDesc
