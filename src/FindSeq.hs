{-# LANGUAGE LambdaCase #-}
module FindSeq where

import Data.List        (sort)
import System.Directory
import System.FilePath

find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else loop fs'
  where
    loop [] = return Nothing
    loop (f:fs) = do
      let d' = d </> f
      isdir <- doesDirectoryExist d'
      if isdir
        then do
         r <- find s d'
         case r of
           Just _ -> return r
           Nothing -> loop fs
        else loop fs
