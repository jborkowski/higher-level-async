module Lib where

import           AsyncAPI
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit

getURL :: String -> IO B.ByteString
getURL url = BL.toStrict <$> simpleHttp url

fetchWiki = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
