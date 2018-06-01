{-# LANGUAGE OverloadedStrings #-}

import           Data.Proxy
import           Servant.Docs            (docs, pretty)
import qualified Servant.Docs.Pandoc     (pandoc)
import           Text.Pandoc             (Pandoc, def, writeHtmlString)

import           Zelinf.StoppingTime.API (API)

main :: IO ()
main = do
  let docStr = writeHtmlString def pandoc
  putStr docStr

pandoc :: Pandoc
pandoc = Servant.Docs.Pandoc.pandoc . docs . pretty $ (Proxy :: Proxy API)
