module Main where

import qualified Data.ByteString    as BS
import           FindSeq
import           Lib
import           System.Environment

main :: IO ()
main = do
  [s, d] <- getArgs
  result <- find s d
  print result
