module Main where

import qualified Data.ByteString as BS
import           Lib

main :: IO ()
main = do
  xs <- fetchList sites
  print (map BS.length xs)
