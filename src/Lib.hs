module Lib where

import           AsyncAPI             (wait, withAsync)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit

getURL :: String -> IO BS.ByteString
getURL url = BL.toStrict <$> simpleHttp url

fetchWiki =
  withAsync (getURL "http://www.wikipedia.org/wiki/Shovel") $ \a1 ->
  withAsync (getURL "http://www.wikipedia.org/wiki/Spade") $ \a2 -> do
  r1 <- wait a1
  r2 <- wait a2
  print (BS.length r1, BS.length r2)
