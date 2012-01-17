module Watcher (start) where

import Data.List
import Control.Monad
import qualified Control.Monad.Parallel as MP
import Control.Concurrent
import System.Posix.Files
import System.Posix.Types
import System.Posix.Syslog
import System.Directory
import System.FilePath.Posix
import qualified System.INotify as IN

type FileSet = (Int, EpochTime, FilePath)

start:: FilePath -> Int -> () -> IO ()
start p s _ = do
    syslog Info ("Watching directory: '" ++ p ++ "' with size limit: '" ++ show s ++ "MB'")
    IN.withINotify (watch p s)
        
watch:: FilePath -> Int -> IN.INotify -> IO()
watch p s n = do
    _ <- IN.addWatch n [IN.Create,IN.Modify] p (handler p s)
    forever $ threadDelay 10000 
        
handler:: FilePath -> Int -> IN.Event -> IO ()
handler p s evt = do
    syslog Debug $ "Event: " ++ show evt
    coll <- collectDir p
    let sf = sortBy (\(_,a,_) (_,b,_) -> compare a b) coll
    syslog Debug $ "Size: " ++ show (size sf)
    clean sf
    where
        size = foldl (\acc (sz,_,_) -> sz+acc) 0
        clean l 
          | size l < (s*1024*1024) = return ()
          | otherwise  = do let (_,_,f) = head l
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
                     if can then collectDir p `mplus` (return a)
                            else return $ (size st, accessTime st, p):a
                        
