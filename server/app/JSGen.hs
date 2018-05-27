import           Data.Proxy              (Proxy (..))
import qualified Data.Text.IO            as Text
import           Servant.JS

import           Zelinf.StoppingTime.API

main :: IO ()
main = Text.putStrLn $
  jsForAPI (Proxy :: Proxy API) jquery
