module Lib where

import           AsyncAPI             (concurrently, wait)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit

getURL :: String -> IO BS.ByteString
getURL url = BL.toStrict <$> simpleHttp url

fetchWiki = do
  (r1, r2) <- concurrently
    (getURL "http://www.wikipedia.org/wiki/Shovel")
    (getURL "http://www.wikipedia.org/wiki/Spade")
  print (BS.length r1, BS.length r2)
