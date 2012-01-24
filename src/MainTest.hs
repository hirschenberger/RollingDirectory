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

import Test.HUnit

import Data.List (sort, sortBy)
import Control.Monad (liftM, replicateM)
import Control.Exception (bracket)
import System.Directory (getTemporaryDirectory, createDirectory, removeDirectoryRecursive)
import System.FilePath.Posix ((</>))
import System.IO (openBinaryTempFile, hPutBuf, hClose)
import Foreign.Marshal.Alloc (mallocBytes, free)

import Watcher

main:: IO()
main = TF.defaultMain tests

tests:: [TF.Test]
tests = [testCase "T1" (bracket (do files <- createTestFiles 100 100000
                                    wfiles <- (collectDir =<< testDir)
                                    let sw = sortBy (\(_,_,a) (_,_,b) -> compare a b) wfiles
                                    return (sort files, sw))
                                 (\(_, _) -> cleanup) -- replace with 'return ()' for debugging purposes
                                 (\(f, w) -> assertBool "Directory scanning" $ all (\(a, (_,_,b)) -> a == b) (zip f w)))
        ]

createTestFiles:: Int -> Int -> IO [FilePath]
createTestFiles num size = do
    cleanup
    dir <- testDir
    payload <- mallocBytes size
    createDirectory dir
    names <- replicateM num (create dir payload)
    free payload
    return names
    where
      create d p = bracket (openBinaryTempFile d "testfile_.bin")
                           (\(_, hd) -> hClose hd)
                           (\(f, hd) -> hPutBuf hd p size >> return f)
                                           
testDir:: IO FilePath
testDir = liftM (</> "RD_test") getTemporaryDirectory

cleanup:: IO()
cleanup = catch (removeDirectoryRecursive =<< testDir) (\_ -> return())