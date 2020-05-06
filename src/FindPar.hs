{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module FindPar where

import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Monad.IO.Class
import           Control.Monad.Par.Class
import           Control.Monad.Par.IO
import           Data.IORef
import           Data.List               (sort)
import           System.Directory
import           System.Environment
import           System.FilePath
import GHC.Conc (getNumCapabilities)


main :: IO ()
main = do
  [s, d] <- getArgs
  runParIO (find s d) >>= print

find :: String -> FilePath -> ParIO (Maybe FilePath)
find s d = do
  fs <- liftIO $ getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
    let ps = map (d </>) fs'
    foldr (subfind s) dowait ps []
  where
    dowait as = loop (reverse as)

    loop [] = return Nothing
    loop (a:as) = do
      r <- get a
      case r of
        Nothing -> loop as
        Just a  -> return (Just a)

subfind :: String -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        -> [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
subfind s p inner ivars = do
  isdir <- liftIO $ doesDirectoryExist p
  if not isdir
    then inner ivars
    else do v <- new
            fork (find s p >>= put v)
            inner (v : ivars)
