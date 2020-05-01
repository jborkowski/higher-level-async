module Lib where

import           AsyncAPI             (concurrently, wait)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit

type URL = String

getURL :: String -> IO BS.ByteString
getURL url = BL.toStrict <$> simpleHttp url

sites :: [URL]
sites = [ "http://www.wikipedia.org/wiki/Shovel"
        , "http://www.wikipedia.org/wiki/Spade"
        ]

fetchWiki = do
  (r1, r2) <- concurrently
    (getURL "http://www.wikipedia.org/wiki/Shovel")
    (getURL "http://www.wikipedia.org/wiki/Spade")
  print (BS.length r1, BS.length r2)

fetchList :: [URL] -> IO [BS.ByteString]
fetchList sites =
  foldr conc (return []) (map getURL sites)
    where
      conc ioa ioas = do
        (a,as) <- concurrently ioa ioas
        return (a:as)

