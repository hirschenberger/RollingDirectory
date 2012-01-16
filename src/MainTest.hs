
module MainTest where

import Test.HUnit

testMain :: IO Counts
testMain = runTestTT $ TestList []
