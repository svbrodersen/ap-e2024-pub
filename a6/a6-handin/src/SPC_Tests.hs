module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 5000000) $
    testGroup
      "SPC (core)"
      [ testCase "Add 1 worker test" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (writeIORef ref True) longTimeout
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
          j2 <- jobAdd spc $ Job (writeIORef ref False) longTimeout
          r3 <- jobStatus spc j2
          r3 @?= JobRunning
          r4 <- jobWait spc j2
          r4 @?= Just Done
          v2 <- readIORef ref
          v2 @?= False
      , testCase "Add multiple workers test" $ do
          spc <- startSPC
          ref <- newIORef False
          j1 <- jobAdd spc $ Job (threadDelay 100 >> writeIORef ref True) longTimeout
          j2 <- jobAdd spc $ Job (threadDelay 100 >> writeIORef ref True) longTimeout
          j3 <- jobAdd spc $ Job (threadDelay 100 >> writeIORef ref True) longTimeout
          j4 <- jobAdd spc $ Job (threadDelay 100000 >> writeIORef ref False) longTimeout
          r1 <- jobStatus spc j1
          r1 @?= JobPending
          r2 <- jobStatus spc j2
          r2 @?= JobPending
          r3 <- jobStatus spc j3
          r3 @?= JobPending
          r4 <- jobStatus spc j4
          r4 @?= JobPending
          w1 <- workerAdd spc "worker1"
          w1 @?= Right "Successfully added worker 'worker1'."
          w2 <- workerAdd spc "worker2"
          w2 @?= Right "Successfully added worker 'worker2'."
          w3 <- workerAdd spc "worker3"
          w3 @?= Right "Successfully added worker 'worker3'."
          w4 <- workerAdd spc "worker4"
          w4 @?= Right "Successfully added worker 'worker4'."
          w5 <- workerAdd spc "worker1"
          w5 @?= Left "Worker with name 'worker1' already exists."
          r5 <- jobStatus spc j1
          r5 @?= JobRunning
          r6 <- jobStatus spc j2
          r6 @?= JobRunning
          r7 <- jobStatus spc j3
          r7 @?= JobRunning
          r8 <- jobWait spc j3
          r8 @?= Just Done
          v1 <- readIORef ref
          v1 @?= True
          r9 <- jobWait spc j4
          r9 @?= Just Done
          v2 <- readIORef ref
          v2 @?= False
      , testCase "Cancel Job test" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 10000000 >> writeIORef ref True) smallTimeout
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled
      , -- Task 4 Timeouts
        testCase "Timeouts Test" $ do
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 10000000 >> writeIORef ref True) smallTimeout
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout
      , -- Task 5 Exceptions
        testCase "Exceptions Test" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (error "Exception") longTimeout
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneCrashed
      , -- Task 6 Remove Workers
        testCase "Remove Worker Test" $ do
          spc <- startSPC
          ref <- newIORef False
          let workerName = "worker1"
          worker <- workerAdd spc workerName
          worker @?= Right ("Successfully added worker '" ++ workerName ++ "'.")
          j1 <- jobAdd spc $ Job (threadDelay longTimeout >> writeIORef ref True) longTimeout
          r1 <- jobStatus spc j1
          r1 @?= JobRunning
          workerStop spc workerName
          r2 <- jobStatus spc j1
          r2 @?= JobDone DoneCancelled
          v <- readIORef ref
          v @?= False
      ]
 where
  smallTimeout = 1
  longTimeout = 10000
