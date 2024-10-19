module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testGroup
          "Worker Test"
          [ testCase "Single Worker" $
              do
                spc <- startSPC
                ref <- newIORef False
                j <- jobAdd spc $ Job (writeIORef ref True) 1
                r2 <- workerAdd spc "w1"
                r2 @?= "Added worker: w1"
                -- r3 <- jobStatus spc j
                -- r3 @?= JobRunning
                r4 <- workerAdd spc "w1"
                r4 @?= "Worker name already in use"
                v <- readIORef ref
                v @?= True
          ]
      ]
