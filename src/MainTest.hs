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

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit

import Test.HUnit (Assertion, assertBool, (@?=))

import Data.List (sort, sortBy)
import Control.Monad (liftM, replicateM)
import qualified Control.Exception as CE
import Control.Concurrent (forkIO, threadDelay, killThread)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath.Posix ((</>))
import System.IO (openBinaryTempFile, hPutBuf, hFlush, hClose)
import Foreign.Marshal.Alloc (mallocBytes, free)

import qualified Watcher as W
import qualified Utils as U

main:: IO()
main = TF.defaultMain tests

tests:: [TF.Test]
tests = [testCase "Directory scanner" directoryScannerTest,
         testCase "Watcher" watcherTest,
         testCase "Size parser" sizeParserTest
        ]

directoryScannerTest:: IO()
directoryScannerTest = CE.bracket scan
                                  (\(_, _) -> cleanup) -- replace with 'return ()' for debugging purposes
                                  (\(f, w) -> assertBool "Directory scanning" $ check f w)
                        where
                            scan = do files <- createTestFiles 100 100000
                                      wfiles <- (W.collectDir =<< testDir)
                                      let sw = sortBy (\(_,_,a) (_,_,b) -> compare a b) wfiles
                                      return (sort files, sw)
                            check f w = all (\(a, (_,_,b)) -> a == b) (zip f w)

watcherTest:: IO()
watcherTest = CE.bracket watch
                         (\_ -> cleanup)
                         (\s -> assertBool "Size below limit" $ s < limit)
                    where
                      limit:: Int
                      limit = 100000
                      watch = do _ <- createTestFiles 100 1 
                                 path <- testDir
                                 t <- forkIO( W.start path limit () )
                                 threadDelay 300000
                                 _ <- createTestFiles 100 10000
                                 threadDelay 300000
                                 _ <- createTestFiles 100 10000
                                 threadDelay 300000
                                 killThread t
                                 files <- W.collectDir path
                                 let size = foldl (\acc (sz,_,_) -> sz+acc) 0 files
                                 return (size)

sizeParserTest:: Assertion
sizeParserTest = (U.parseSize "1" @?= Just 1) >>
                 (U.parseSize "1MB" @?= Just 1024) >>
                 (U.parseSize "1GB" @?= Just (1024*1024)) >>
                 (U.parseSize "1TB" @?= Just (1024*1024*1024)) >>
                 (U.parseSize " 1MB " @?= Just 1024) >>
                 (U.parseSize " 1 MB " @?= Just 1024) >>
                 (U.parseSize "0" @?= Just 0) >>
                 (U.parseSize " 1 " @?= Just 1) >>
                 (U.parseSize "" @?= Nothing) >>
                 (U.parseSize "XXX" @?= Nothing)

createTestFiles:: Int -> Int -> IO [FilePath]
createTestFiles num size = do
    dir <- testDir
    payload <- mallocBytes size
    createDirectoryIfMissing True dir
    names <- replicateM num (create dir payload)
    free payload
    return names
    where
      create d p = CE.bracket (openBinaryTempFile d "testfile_.bin")
                              (\(_, hd) -> hClose hd)
                              (\(f, hd) -> hPutBuf hd p size >> hFlush hd >> return f)
                                           
testDir:: IO FilePath
testDir = liftM (</> "RD_test") getTemporaryDirectory

cleanup:: IO()
cleanup = CE.catch (removeDirectoryRecursive =<< testDir) 
                   ((\_ -> return()) :: CE.IOException -> IO())
