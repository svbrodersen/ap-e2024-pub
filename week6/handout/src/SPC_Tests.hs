module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "Ping Test" $
          do
            s <- startSPC
            x <- pingSPC s
            x @?= 0
            y <- pingSPC s
            y @?= 1
            v <- pingSPC s
            v @?= 2
      ]
