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
      [ 
        -- Task 2 Adding workers
        testCase "Add worker test" $ do 
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          r3 <- jobStatus spc j2
          r3 @?= JobRunning
          r4 <- jobWait spc j2
          r4 @?= Just Done
          v2 <- readIORef ref
          v2 @?= False,
        -- Task 3 Job Cancellation
        testCase "Cancel Job test" $ do 
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 10000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled,
        -- Task 4 Timeouts
        testCase "Timeouts Test" $ do 
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (threadDelay 10000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout,
        -- Task 5 Exceptions
        testCase "Exceptions Test" $ do 
          spc <- startSPC
          ref <- newIORef False
          _ <- workerAdd spc "worker1"
          j <- jobAdd spc $ Job (error "Exception") 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneCrashed,
        -- Task 6 Remove Workers
        testCase "Remove Worker Test" $ do
          spc <- startSPC
          ref <- newIORef False
          worker <- workerAdd spc "worker1"
          worker1 <- case worker of
            Left err -> error ("Failed to add worker: " ++ err)
            Right w -> return w
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True
          workerStop worker1
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          r3 <- jobStatus spc j2
          r3 @?= JobRunning
          r4 <- jobWait spc j2
          r4 @?= Just DoneCancelled
          v2 <- readIORef ref
          v2 @?= True
      ]