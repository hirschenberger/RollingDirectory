{-
  RollingDirectory is a daemon watching a directory limiting it's size
  Copyright (C) 2011  Falco Hirschenberger <hirsch@bogfoot.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Watcher where

import           Control.Monad
import qualified Control.Monad.Parallel    as MP
import           Data.List
import           System.Directory
import           System.FilePath.Posix
import qualified System.Hiernotify.Polling as HN
import           System.Posix.Files
import           System.Posix.Syslog
import           System.Posix.Types

type FileSet = (Int, EpochTime, FilePath)

start:: FilePath -> Int -> () -> IO ()
start p s _ = do
    syslog Info ("Watching directory: '" ++ p ++ "' with size limit: '" ++ show s ++ "KB'")
    notifier <- HN.mkPollNotifier 1 (HN.Configuration p 5 (not . isPrefixOf "."))
    watch p s notifier

watch:: FilePath -> Int -> HN.Notifier -> IO()
watch p s n = do
    forever $ do
      (d, _) <- HN.difference n
      handler p s d


handler:: FilePath -> Int -> HN.Difference -> IO ()
handler p s d = do
    syslog Debug $ "Event: " ++ show d
    coll <- collectDir p
    let sf = sortBy (\(_,a,_) (_,b,_) -> compare a b) coll
    syslog Debug $ "Size: " ++ show (size sf)
    clean sf
    where
        size = foldl (\acc (sz,_,_) -> sz+acc) 0
        clean l
          | size l < s = return ()
          | otherwise  = do let (_,_,f) = head l
                            syslog Info ("Deleting file: " ++ f)
                            removeFile f
                            clean $ tail l

collectDir:: FilePath -> IO [FileSet]
collectDir path = do
    files <- getDirectoryContents path
    l <- MP.mapM (go []) files
    return $ concat  l
    where
        isDirAndAccess:: FilePath -> FileStatus -> IO Bool
        isDirAndAccess p s = liftM2 (&&) (fileAccess p False False True)
                                         (return $ isDirectory s)
        size:: FileStatus -> Int
        size = fromIntegral . fileSize
        go:: [FileSet] -> FilePath -> IO [FileSet]
        go a []   = return a
        go a "."  = return a
        go a ".." = return a
        go a f =  do let p = path </> f
                     st <- getFileStatus p
                     can <- isDirAndAccess p st
                     if can then collectDir p `mplus` return a
                            else return $ (size st, accessTime st, p):a

