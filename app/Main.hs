module Main where

import qualified Data.ByteString as BS
import           Lib
import           Server

main :: IO ()
main = do
  withServer
  xs <- fetchList sites
  print (map BS.length xs)
